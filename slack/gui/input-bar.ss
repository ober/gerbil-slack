;; -*- Gerbil -*-
;; Message input bar: send messages, Enter/Shift+Enter, thread replies

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/api/chat
  :ober/slack/gui/app
  :ober/slack/gui/channel-view)

(export #t)

;;;; State

(def *thread-ts* #f)  ;; when set, replies go to this thread

;;;; Public API

(def (input-bar-init!)
  "Wire up the input bar: Enter sends, Shift+Enter adds newline, send button."
  ;; Wire Enter key on the input widget
  (when *message-input*
    (qt-on-key-press! *message-input*
      (lambda ()
        (let ((key (qt-last-key-code))
              (mods (qt-last-key-modifiers)))
          (when (or (= key QT_KEY_RETURN) (= key QT_KEY_ENTER))
            (unless (> (bitwise-and mods QT_MOD_SHIFT) 0)
              ;; Enter without Shift: send message
              (send-current-message!)))))))
  ;; Wire send button (already wired in app.ss but replace with real logic)
  (when *send-button*
    (qt-on-clicked! *send-button*
      (lambda () (send-current-message!)))))

(def (input-bar-set-thread! ts)
  "Set thread context for replies. Pass #f to clear."
  (set! *thread-ts* ts)
  ;; Update placeholder text
  (when *message-input*
    (if ts
      (qt-plain-text-edit-set-placeholder! *message-input* "Reply in thread...")
      ;; Restore channel placeholder (will be set by channel-view-load!)
      (void))))

(def (input-bar-clear-thread!)
  "Clear thread reply context."
  (input-bar-set-thread! #f))

;;;; Internal

(def (send-current-message!)
  "Send the current input text to the active channel (or thread)."
  (when (and *message-input* *current-channel-id*)
    (let ((text (qt-plain-text-edit-text *message-input*)))
      (when (and text (> (string-length (string-trim text)) 0))
        (let ((trimmed (string-trim text)))
          ;; Clear input immediately (optimistic)
          (qt-plain-text-edit-clear! *message-input*)
          ;; Send in background
          (try
            (let ((ts (if *thread-ts*
                        (chat-post-message *current-channel-id* trimmed
                                           thread-ts: *thread-ts*)
                        (chat-post-message *current-channel-id* trimmed))))
              (when ts
                (set-status! "Message sent"))
              (unless ts
                (set-status! "Message sent (no ts returned)")))
            (catch (e)
              ;; On error, restore the text so user doesn't lose it
              (qt-plain-text-edit-set-text! *message-input* trimmed)
              (set-status! (string-append "Send failed: "
                            (with-exception-catcher
                              (lambda (e2) "unknown error")
                              (lambda () (error-message e))))))))))))

(def (string-trim s)
  "Remove leading and trailing whitespace."
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1))
                    i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref s i)))
                  (loop (- i 1))
                  (+ i 1)))))
    (if (>= start end) "" (substring s start end))))
