;; -*- Gerbil -*-
;; Channel view: message rendering, history loading, scroll behavior

(import
  :std/sugar
  :std/iter
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/cache
  :ober/slack/api/conversations
  :ober/slack/api/users
  :ober/slack/markdown
  :ober/slack/gui/app
  :ober/slack/gui/theme)

(export #t)

;;;; State

(def *current-channel-id* #f)
(def *current-messages* [])     ;; list of message structs, oldest first
(def *oldest-ts* #f)            ;; for pagination (load older)

;;;; Public API

(def (channel-view-load! channel-id)
  "Load and display messages for a channel. Called when user selects a channel."
  (set! *current-channel-id* channel-id)
  (set! *oldest-ts* #f)
  ;; Show loading progress
  (channel-view-show-progress!)
  ;; Update channel header
  (update-channel-header! channel-id)
  ;; Load messages: try cache first, then API
  (let ((messages (load-messages channel-id)))
    (set! *current-messages* messages)
    (when (pair? messages)
      (set! *oldest-ts* (message-ts (car messages))))
    ;; Render all messages
    (render-messages! messages)
    ;; Scroll to bottom
    (when *message-browser*
      (qt-text-browser-scroll-to-bottom! *message-browser*))
    ;; Hide progress
    (channel-view-hide-progress!)))

(def (channel-view-load-older!)
  "Load older messages (pagination). Prepends to current view."
  (when (and *current-channel-id* *oldest-ts*)
    (let ((older (conversations-history *current-channel-id*
                                         limit: 50
                                         latest: *oldest-ts*)))
      (when (pair? older)
        ;; older comes newest-first from API, reverse for display
        (let ((sorted (reverse older)))
          (set! *oldest-ts* (message-ts (car sorted)))
          (set! *current-messages* (append sorted *current-messages*))
          ;; Re-render everything
          (render-messages! *current-messages*))))))

(def (channel-view-append-message! msg)
  "Append a single new message (from real-time events)."
  (set! *current-messages* (append *current-messages* (list msg)))
  (when *message-browser*
    (qt-text-browser-append! *message-browser* (render-message-html msg))
    (qt-text-browser-scroll-to-bottom! *message-browser*)))

(def (channel-view-update-message! ts new-msg)
  "Update an existing message (edited). Re-renders all messages."
  (set! *current-messages*
    (map (lambda (m) (if (equal? (message-ts m) ts) new-msg m))
         *current-messages*))
  (render-messages! *current-messages*))

(def (channel-view-delete-message! ts)
  "Remove a message from the view."
  (set! *current-messages*
    (filter (lambda (m) (not (equal? (message-ts m) ts)))
            *current-messages*))
  (render-messages! *current-messages*))

;;;; Internal: Message Loading

(def (load-messages channel-id)
  "Load messages from cache, falling back to API. Returns oldest-first."
  (let ((cached (cache-get-messages channel-id 50)))
    (if (pair? cached)
      (reverse cached)  ;; cache returns newest-first
      ;; Fetch from API
      (let ((fresh (conversations-history channel-id limit: 50)))
        ;; Cache them
        (for-each
          (lambda (m)
            (cache-message! (set-message-channel m channel-id)))
          fresh)
        (reverse fresh)))))  ;; API returns newest-first

(def (set-message-channel msg ch-id)
  "Return a copy of msg with the channel field set."
  (make-message
    ts: (message-ts msg)
    channel: ch-id
    user: (message-user msg)
    text: (message-text msg)
    thread-ts: (message-thread-ts msg)
    reply-count: (message-reply-count msg)
    reactions: (message-reactions msg)
    attachments: (message-attachments msg)
    files: (message-files msg)
    edited: (message-edited msg)
    is-bot: (message-is-bot msg)
    bot-id: (message-bot-id msg)))

;;;; Internal: Channel Header

(def (update-channel-header! channel-id)
  "Update the channel header with name and topic."
  (let ((ch (or (cache-get-channel channel-id)
                (try (conversations-info channel-id)
                  (catch (e) #f)))))
    (if ch
      (begin
        (set-channel! (string-append "# " (or (channel-name ch) channel-id))
                      (or (channel-topic ch) ""))
        ;; Update input placeholder
        (when *message-input*
          (qt-plain-text-edit-set-placeholder! *message-input*
            (string-append "Message #" (or (channel-name ch) channel-id)))))
      (set-channel! channel-id ""))))

;;;; Internal: Message Rendering

(def (render-messages! messages)
  "Render all messages as HTML and display in browser."
  (when *message-browser*
    (let ((html (messages->html messages)))
      (qt-text-browser-set-html! *message-browser* html))))

(def (messages->html messages)
  "Convert a list of messages to a single HTML document."
  (string-append
    "<html><body style='margin:0; padding:8px; background-color:" clr-message-bg
    "; color:" clr-message-text "; font-family: -apple-system, sans-serif; font-size:14px;'>"
    (let ((prev-date #f))
      (let loop ((msgs messages) (acc ""))
        (if (null? msgs)
          acc
          (let* ((msg (car msgs))
                 (msg-date (ts->date-string (message-ts msg)))
                 (separator (if (and msg-date (not (equal? msg-date prev-date)))
                              (date-separator-html msg-date)
                              ""))
                 (msg-html (render-message-html msg)))
            (loop (cdr msgs)
                  (string-append acc separator msg-html))))))
    "</body></html>"))

(def (render-message-html msg)
  "Render a single message as HTML."
  (let* ((user-id (message-user msg))
         (username (resolve-username user-id))
         (ts-str (format-timestamp (message-ts msg)))
         (text (or (message-text msg) ""))
         (body-html (mrkdwn->html text))
         (edited-mark (if (message-edited msg)
                        "<span style='color:#616061; font-size:11px;'> (edited)</span>"
                        ""))
         (reactions-html (render-reactions (message-reactions msg)))
         (files-html (render-files (message-files msg)))
         (thread-html (render-thread-indicator msg))
         (bot-badge (if (message-is-bot msg)
                      "<span style='background:#E8E8E8; color:#616061; font-size:10px; padding:1px 4px; border-radius:3px; margin-left:4px;'>APP</span>"
                      "")))
    (string-append
      "<div style='padding:6px 16px; border-bottom:1px solid #F5F5F5;'>"
      ;; Username + timestamp row
      "<div>"
      "<span style='font-weight:bold; color:" clr-message-text ";'>"
      (html-escape username) "</span>"
      bot-badge
      "<span style='color:" clr-message-text-dim "; font-size:11px; margin-left:8px;'>"
      ts-str "</span>"
      "</div>"
      ;; Message body
      "<div style='margin-top:2px;'>" body-html edited-mark "</div>"
      ;; Files
      files-html
      ;; Reactions
      reactions-html
      ;; Thread indicator
      thread-html
      "</div>")))

(def (render-reactions reactions)
  "Render reaction badges as HTML."
  (if (or (not reactions) (null? reactions))
    ""
    (string-append
      "<div style='margin-top:4px;'>"
      (apply string-append
        (map (lambda (r)
               (string-append
                 "<span style='display:inline-block; background:#F0F0F0; border:1px solid #E0E0E0; "
                 "border-radius:12px; padding:2px 6px; margin-right:4px; font-size:12px;'>"
                 ":" (or (reaction-name r) "?") ": "
                 (number->string (or (reaction-count r) 0))
                 "</span>"))
             reactions))
      "</div>")))

(def (render-files files)
  "Render file attachment indicators."
  (if (or (not files) (null? files))
    ""
    (string-append
      "<div style='margin-top:4px;'>"
      (apply string-append
        (map (lambda (f)
               (let ((name (or (file-info-name f) "file"))
                     (size (file-info-size f)))
                 (string-append
                   "<div style='background:#F8F8F8; border:1px solid #E0E0E0; "
                   "border-radius:4px; padding:4px 8px; margin-top:2px; font-size:12px;'>"
                   "&#128206; " (html-escape name)
                   (if size
                     (string-append " <span style='color:#616061;'>(" (format-size size) ")</span>")
                     "")
                   "</div>")))
             files))
      "</div>")))

(def (render-thread-indicator msg)
  "Render thread reply count indicator."
  (let ((count (message-reply-count msg)))
    (if (and count (> count 0))
      (string-append
        "<div style='margin-top:4px; font-size:12px; color:" clr-accent-blue ";'>"
        (number->string count) " "
        (if (= count 1) "reply" "replies")
        "</div>")
      "")))

;;;; Internal: Formatting Utilities

(def (resolve-username user-id)
  "Resolve a user ID to a display name."
  (if (not user-id) "Unknown"
    (let ((cached (cache-get-user user-id)))
      (if cached
        (user-display cached)
        (or (user-name-for-id user-id) user-id)))))

(def (format-timestamp ts)
  "Format a Slack timestamp string to 'HH:MM'."
  (if (not ts) ""
    (let ((epoch (string->number ts)))
      (if (not epoch) ""
        (let* ((secs (inexact->exact (floor epoch)))
               (secs-in-day (modulo secs 86400))
               (hours (quotient secs-in-day 3600))
               (minutes (quotient (modulo secs-in-day 3600) 60)))
          (string-append
            (if (< hours 10) "0" "") (number->string hours) ":"
            (if (< minutes 10) "0" "") (number->string minutes)))))))

(def (ts->date-string ts)
  "Convert Slack timestamp to a date string like '2025-02-07'."
  (if (not ts) #f
    (let ((epoch (string->number ts)))
      (if (not epoch) #f
        (let* ((secs (inexact->exact (floor epoch)))
               ;; Approximate date: days since epoch
               (days (quotient secs 86400))
               ;; Simple Gregorian calculation from days since 1970-01-01
               (year+doy (days->year-doy days)))
          (let ((year (car year+doy))
                (month-day (doy->month-day (cdr year+doy) (leap-year? (car year+doy)))))
            (string-append
              (number->string year) "-"
              (pad2 (car month-day)) "-"
              (pad2 (cdr month-day)))))))))

(def (days->year-doy days)
  "Convert days since 1970-01-01 to (year . day-of-year)."
  (let loop ((y 1970) (d days))
    (let ((yd (if (leap-year? y) 366 365)))
      (if (< d yd)
        (cons y d)
        (loop (+ y 1) (- d yd))))))

(def (leap-year? y)
  (and (zero? (modulo y 4))
       (or (not (zero? (modulo y 100)))
           (zero? (modulo y 400)))))

(def (doy->month-day doy leap?)
  "Convert day-of-year (0-based) to (month . day), both 1-based."
  (let ((mdays (if leap?
                 [31 29 31 30 31 30 31 31 30 31 30 31]
                 [31 28 31 30 31 30 31 31 30 31 30 31])))
    (let loop ((m 1) (d (+ doy 1)) (ms mdays))
      (if (or (null? ms) (<= d (car ms)))
        (cons m d)
        (loop (+ m 1) (- d (car ms)) (cdr ms))))))

(def (pad2 n)
  "Pad a number to 2 digits."
  (let ((s (number->string n)))
    (if (< (string-length s) 2) (string-append "0" s) s)))

(def (date-separator-html date-str)
  "HTML for a date separator line."
  (string-append
    "<div style='text-align:center; padding:8px 0; margin:8px 0; "
    "border-bottom:1px solid #E0E0E0; font-size:12px; font-weight:bold; "
    "color:" clr-message-text-dim ";'>"
    date-str "</div>"))

(def (format-size bytes)
  "Format file size in human-readable form."
  (cond
    ((not bytes) "")
    ((< bytes 1024) (string-append (number->string bytes) " B"))
    ((< bytes (* 1024 1024))
     (string-append (number->string (quotient bytes 1024)) " KB"))
    (else
     (let ((mb (/ bytes (* 1024.0 1024.0))))
       (string-append
         (number->string (/ (floor (* mb 10.0)) 10.0)) " MB")))))

(def (html-escape s)
  "Escape HTML special characters."
  (let loop ((i 0) (acc ""))
    (if (>= i (string-length s))
      acc
      (let ((c (string-ref s i)))
        (loop (+ i 1)
              (string-append acc
                (case c
                  ((#\<) "&lt;")
                  ((#\>) "&gt;")
                  ((#\&) "&amp;")
                  ((#\") "&quot;")
                  (else (string c)))))))))

;;;; Clipboard Support

(def (channel-view-copy-selected!)
  "Copy selected text from the message browser to clipboard."
  (when *message-browser*
    (let ((text (qt-text-browser-plain-text *message-browser*)))
      (when (and text (> (string-length text) 0))
        (qt-clipboard-set-text! *app* text)
        (set-status! "Copied to clipboard")))))

;;;; Progress Indication

(def (channel-view-show-progress!)
  "Show loading state."
  (when *progress-bar*
    (qt-progress-bar-set-range! *progress-bar* 0 0)  ;; indeterminate
    (qt-widget-show! *progress-bar*))
  (set-status! "Loading messages..."))

(def (channel-view-hide-progress!)
  "Hide loading state."
  (when *progress-bar*
    (qt-progress-bar-set-range! *progress-bar* 0 100)
    (qt-progress-bar-set-value! *progress-bar* 100)
    (qt-widget-hide! *progress-bar*)))
