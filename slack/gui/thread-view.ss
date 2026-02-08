;; -*- Gerbil -*-
;; Thread view: side panel for viewing and replying to message threads

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/cache
  :ober/slack/api/conversations
  :ober/slack/api/chat
  :ober/slack/markdown
  :ober/slack/gui/app
  :ober/slack/gui/theme
  :ober/slack/gui/channel-view)

(export #t)

;;;; State

(def *thread-panel* #f)       ;; the panel widget
(def *thread-browser* #f)     ;; QTextBrowser for thread messages
(def *thread-input* #f)       ;; QPlainTextEdit for thread reply
(def *thread-send-btn* #f)    ;; Send button
(def *thread-close-btn* #f)   ;; Close button
(def *thread-channel-id* #f)  ;; channel containing the thread
(def *thread-ts* #f)          ;; parent message ts
(def *thread-messages* [])    ;; list of message structs in thread

;;;; Public API

(def (thread-view-build! parent-splitter)
  "Build the thread panel widget and add to the splitter. Starts hidden."
  (let* ((panel (qt-widget-create))
         (layout (qt-vbox-layout-create panel))
         ;; Header with close button
         (header (qt-widget-create))
         (header-layout (qt-hbox-layout-create header))
         (title-label (qt-label-create "Thread"))
         (close-btn (qt-push-button-create "X"))
         ;; Thread messages browser
         (browser (qt-text-browser-create))
         ;; Reply input area
         (input-area (qt-widget-create))
         (input-layout (qt-hbox-layout-create input-area))
         (input (qt-plain-text-edit-create))
         (send-btn (qt-push-button-create "Reply")))

    ;; Store references
    (set! *thread-panel* panel)
    (set! *thread-browser* browser)
    (set! *thread-input* input)
    (set! *thread-send-btn* send-btn)
    (set! *thread-close-btn* close-btn)

    ;; Style header
    (qt-widget-set-style-sheet! header
      (string-append "background-color:" clr-message-bg
                     "; border-bottom: 1px solid " clr-message-border ";"))
    (qt-widget-set-style-sheet! title-label
      (string-append "font-weight:bold; font-size:14px; color:" clr-message-text
                     "; background:transparent; padding:8px;"))
    (qt-widget-set-style-sheet! close-btn
      (string-append "QPushButton { background:transparent; border:none; "
                     "font-weight:bold; font-size:14px; color:" clr-message-text-dim
                     "; padding:8px; }"
                     "QPushButton:hover { color:" clr-accent-red "; }"))

    ;; Style browser
    (style-message-area! browser)
    (qt-text-browser-set-open-external-links! browser #t)

    ;; Style input
    (style-message-input! input)
    (qt-plain-text-edit-set-placeholder! input "Reply in thread...")
    (qt-widget-set-maximum-height! input 60)
    (style-send-button! send-btn)

    ;; Assemble header
    (qt-layout-add-widget! header-layout title-label)
    (qt-layout-add-stretch! header-layout)
    (qt-layout-add-widget! header-layout close-btn)
    (qt-layout-set-margins! header-layout 0 0 0 0)

    ;; Assemble input area
    (qt-layout-add-widget! input-layout input)
    (qt-layout-add-widget! input-layout send-btn)
    (qt-layout-set-margins! input-layout 8 8 8 8)
    (qt-layout-set-spacing! input-layout 8)

    ;; Assemble main layout
    (qt-layout-add-widget! layout header)
    (qt-layout-add-widget! layout browser)
    (qt-layout-add-widget! layout input-area)
    (qt-layout-set-stretch-factor! layout 1 1)
    (qt-layout-set-margins! layout 0 0 0 0)
    (qt-layout-set-spacing! layout 0)

    ;; Set minimum width
    (qt-widget-set-minimum-width! panel 300)

    ;; Wire close button
    (qt-on-clicked! close-btn (lambda () (thread-view-close!)))

    ;; Wire send button
    (qt-on-clicked! send-btn (lambda () (thread-send-reply!)))

    ;; Wire Enter key on input
    (qt-on-key-press! input
      (lambda ()
        (let ((key (qt-last-key-code))
              (mods (qt-last-key-modifiers)))
          (when (or (= key QT_KEY_RETURN) (= key QT_KEY_ENTER))
            (unless (> (bitwise-and mods QT_MOD_SHIFT) 0)
              (thread-send-reply!))))))

    ;; Add to splitter, hidden initially
    (qt-splitter-add-widget! parent-splitter panel)
    (qt-widget-hide! panel)

    panel))

(def (thread-view-open! channel-id thread-ts)
  "Open the thread panel for a given message thread."
  (set! *thread-channel-id* channel-id)
  (set! *thread-ts* thread-ts)
  ;; Load thread messages
  (let ((messages (load-thread-messages channel-id thread-ts)))
    (set! *thread-messages* messages)
    (render-thread-messages! messages))
  ;; Show panel
  (when *thread-panel*
    (qt-widget-show! *thread-panel*))
  ;; Clear input
  (when *thread-input*
    (qt-plain-text-edit-clear! *thread-input*)))

(def (thread-view-close!)
  "Close the thread panel."
  (when *thread-panel*
    (qt-widget-hide! *thread-panel*))
  (set! *thread-channel-id* #f)
  (set! *thread-ts* #f)
  (set! *thread-messages* []))

(def (thread-view-open?)
  "Check if thread panel is currently visible."
  (and *thread-panel* (qt-widget-visible? *thread-panel*)))

(def (thread-view-append-reply! msg)
  "Append a new reply to the thread view (from real-time events)."
  (when (and *thread-ts*
             (equal? (message-thread-ts msg) *thread-ts*))
    (set! *thread-messages* (append *thread-messages* (list msg)))
    (when *thread-browser*
      (qt-text-browser-append! *thread-browser* (render-message-html msg))
      (qt-text-browser-scroll-to-bottom! *thread-browser*))))

;;;; Internal

(def (load-thread-messages channel-id thread-ts)
  "Load thread replies from cache, then API."
  (let ((cached (cache-get-thread channel-id thread-ts)))
    (if (pair? cached)
      cached
      (conversations-replies channel-id thread-ts))))

(def (render-thread-messages! messages)
  "Render all thread messages in the browser."
  (when *thread-browser*
    (let ((html (string-append
                  "<html><body style='margin:0; padding:8px; background-color:" clr-message-bg
                  "; color:" clr-message-text "; font-family:-apple-system,sans-serif; font-size:14px;'>"
                  (apply string-append
                    (map (lambda (msg)
                           (let ((is-parent (equal? (message-ts msg) *thread-ts*)))
                             (string-append
                               (if is-parent
                                 "<div style='border-bottom:2px solid #E0E0E0; margin-bottom:8px; padding-bottom:8px;'>"
                                 "<div style='padding:4px 0;'>")
                               (render-message-html msg)
                               "</div>")))
                         messages))
                  "</body></html>")))
      (qt-text-browser-set-html! *thread-browser* html)
      (qt-text-browser-scroll-to-bottom! *thread-browser*))))

(def (thread-send-reply!)
  "Send a reply in the current thread."
  (when (and *thread-input* *thread-channel-id* *thread-ts*)
    (let ((text (qt-plain-text-edit-text *thread-input*)))
      (when (and text (> (string-length text) 0))
        (qt-plain-text-edit-clear! *thread-input*)
        (try
          (chat-post-message *thread-channel-id* text
                             thread-ts: *thread-ts*)
          (set-status! "Thread reply sent")
          (catch (e)
            (qt-plain-text-edit-set-text! *thread-input* text)
            (set-status! "Thread reply failed")))))))
