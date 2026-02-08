;; -*- Gerbil -*-
;; Real-time event bridge: Socket Mode -> Qt GUI thread
;; Events arrive on background thread, get queued, and dispatched on Qt main thread via timer.

(import
  :std/sugar
  :std/iter
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/events
  :ober/slack/socket
  :ober/slack/cache
  :ober/slack/gui/app
  :ober/slack/gui/channel-view
  :ober/slack/gui/sidebar)

(export #t)

;;;; Thread-safe event queue

(def *event-queue* [])
(def *event-mutex* (make-mutex 'event-queue))
(def *poll-timer* #f)
(def *poll-timer-handler* #f)  ;; handler ID for cleanup

(def (enqueue-gui-event! thunk)
  "Push a GUI update thunk onto the queue (called from background thread)."
  (mutex-lock! *event-mutex*)
  (unwind-protect
    (set! *event-queue* (append *event-queue* (list thunk)))
    (mutex-unlock! *event-mutex*)))

(def (drain-gui-events!)
  "Pop all pending thunks and execute them on the Qt main thread."
  (let ((thunks []))
    (mutex-lock! *event-mutex*)
    (unwind-protect
      (begin
        (set! thunks *event-queue*)
        (set! *event-queue* []))
      (mutex-unlock! *event-mutex*))
    (for-each (lambda (t) (try (t) (catch (e) (void)))) thunks)))

;;;; Public API

(def (realtime-start!)
  "Start Socket Mode and begin routing events to GUI."
  ;; Register event handlers that queue GUI updates
  (register-gui-event-handlers!)
  ;; Clean up previous timer handler if re-starting
  (when (and *poll-timer* *poll-timer-handler*)
    (unregister-qt-handler! *poll-timer-handler*)
    (qt-timer-stop! *poll-timer*))
  ;; Start polling timer (100ms interval)
  (let ((timer (or *poll-timer* (qt-timer-create))))
    (set! *poll-timer* timer)
    (qt-timer-set-interval! timer 100)
    (set! *poll-timer-handler*
      (qt-on-timeout! timer (lambda () (drain-gui-events!))))
    (qt-timer-start! timer 100))
  ;; Start Socket Mode
  (let ((app-token (getenv "SLACK_APP_TOKEN" #f)))
    (if app-token
      (begin
        (socket-mode-start! app-token)
        (set-status! "Connecting..."))
      (set-status! "No app token — real-time disabled"))))

(def (realtime-stop!)
  "Stop Socket Mode and polling timer."
  (socket-mode-stop!)
  (when *poll-timer*
    (when *poll-timer-handler*
      (unregister-qt-handler! *poll-timer-handler*)
      (set! *poll-timer-handler* #f))
    (qt-timer-stop! *poll-timer*)
    (set! *poll-timer* #f)))

;;;; Event Handlers
;; These run on the Socket Mode background thread.
;; They enqueue GUI update thunks that run on the main thread.

(def (register-gui-event-handlers!)
  ;; New message
  (on-event 'message
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let ((msg (json->message data))
                (ch (hash-get data 'channel)))
            (enqueue-gui-event!
              (lambda ()
                (when (and ch (equal? ch *current-channel-id*))
                  (channel-view-append-message! msg)))))))))

  ;; Message changed
  (on-event 'message-changed
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let* ((ch (hash-get data 'channel))
                 (msg-data (hash-get data 'message))
                 (ts (and msg-data (hash-get msg-data 'ts))))
            (when (and ch ts msg-data (hash-table? msg-data))
              (let ((new-msg (json->message msg-data)))
                (enqueue-gui-event!
                  (lambda ()
                    (when (equal? ch *current-channel-id*)
                      (channel-view-update-message! ts new-msg)))))))))))

  ;; Message deleted
  (on-event 'message-deleted
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let ((ch (hash-get data 'channel))
                (ts (hash-get data 'deleted_ts)))
            (when (and ch ts)
              (enqueue-gui-event!
                (lambda ()
                  (when (equal? ch *current-channel-id*)
                    (channel-view-delete-message! ts))))))))))

  ;; Reaction added
  (on-event 'reaction-added
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let ((ch (hash-get data 'item))
                (item-ch (and (hash-table? (hash-get data 'item))
                              (hash-get (hash-get data 'item) 'channel))))
            (when (and item-ch (equal? item-ch *current-channel-id*))
              ;; Re-render all messages to pick up the reaction change
              (enqueue-gui-event!
                (lambda ()
                  (channel-view-load! *current-channel-id*)))))))))

  ;; Reaction removed
  (on-event 'reaction-removed
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let ((item-ch (and (hash-table? (hash-get data 'item))
                              (hash-get (hash-get data 'item) 'channel))))
            (when (and item-ch (equal? item-ch *current-channel-id*))
              (enqueue-gui-event!
                (lambda ()
                  (channel-view-load! *current-channel-id*)))))))))

  ;; Channel created
  (on-event 'channel-created
    (lambda (ev)
      (enqueue-gui-event!
        (lambda () (sidebar-refresh!)))))

  ;; Presence change
  (on-event 'presence-change
    (lambda (ev)
      ;; Future: update presence dot in sidebar
      (void)))

  ;; User typing
  (on-event 'user-typing
    (lambda (ev)
      (let ((data (slack-event-data ev)))
        (when (and data (hash-table? data))
          (let ((ch (hash-get data 'channel))
                (uid (hash-get data 'user)))
            (when (and ch uid (equal? ch *current-channel-id*))
              (let ((name (or (and (cache-get-user uid)
                                   (user-display (cache-get-user uid)))
                              uid)))
                (enqueue-gui-event!
                  (lambda ()
                    (set-status!
                      (string-append name " is typing...")))))))))))

  ;; Connection status
  (on-event 'connected
    (lambda (ev)
      (enqueue-gui-event!
        (lambda ()
          (set-status! "Connected")))))

  (on-event 'disconnected
    (lambda (ev)
      (enqueue-gui-event!
        (lambda ()
          (set-status! "Disconnected")))))

  (on-event 'reconnecting
    (lambda (ev)
      (enqueue-gui-event!
        (lambda ()
          (set-status! "Reconnecting..."))))))
