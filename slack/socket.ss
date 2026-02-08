;; -*- Gerbil -*-
;; Socket Mode WebSocket client for real-time Slack events

(import
  :std/error
  :std/format
  :std/net/request
  :std/net/websocket
  :std/sugar
  :std/text/json
  :ober/slack/config
  :ober/slack/events
  :ober/slack/http)

(export #t)

;;;; Socket Mode State

(def *ws-connection* #f)
(def *ws-thread* #f)
(def *ws-running* #f)
(def *ws-app-token* #f)

;;;; Connection Management

(def (socket-mode-connect app-token)
  "Call apps.connections.open to get a WebSocket URL, then connect.
   app-token: xapp-... token (not the bot token)."
  (let* ((resp (http-post "https://slack.com/api/apps.connections.open"
                          headers: `(("Authorization" . ,(string-append "Bearer " app-token))
                                     ("Content-Type" . "application/x-www-form-urlencoded"))))
         (body (parameterize ((read-json-key-as-symbol? #t))
                 (call-with-input-string (request-text resp) read-json))))
    (unless (and (hash-table? body) (hash-get body 'ok))
      (error (format "apps.connections.open failed: ~a"
                     (if (hash-table? body) (hash-get body 'error) body))))
    (let ((url (hash-ref body 'url)))
      (websocket-connect url))))

(def (socket-mode-start! app-token)
  "Connect to Socket Mode and begin event loop in background thread.
   app-token: xapp-... token."
  (when *ws-running*
    (socket-mode-stop!))
  (set! *ws-app-token* app-token)
  (set! *ws-running* #t)
  (set! *ws-thread*
        (spawn/name 'socket-mode-loop
                    (lambda () (socket-mode-loop app-token))))
  (emit-event 'connecting #f)
  *ws-thread*)

(def (socket-mode-stop!)
  "Gracefully stop the Socket Mode connection."
  (set! *ws-running* #f)
  (when *ws-connection*
    (with-catch void (lambda () (WebSocket-close *ws-connection*)))
    (set! *ws-connection* #f))
  (emit-event 'disconnected #f))

;;;; Event Loop

(def (socket-mode-loop app-token)
  "Main event loop with auto-reconnection and exponential backoff."
  (let loop ((backoff 1))
    (when *ws-running*
      (let ((connected? (with-catch
                          (lambda (e)
                            (emit-event 'reconnecting
                                        (hash (error (with-output-to-string
                                                       (lambda () (display-exception e))))))
                            #f)
                          (lambda ()
                            (set! *ws-connection* (socket-mode-connect app-token))
                            (emit-event 'connected #f)
                            (ws-receive-loop *ws-connection*)
                            #t))))
        ;; If we get here, connection was lost
        (set! *ws-connection* #f)
        (when *ws-running*
          (let ((wait (min backoff 30)))
            (emit-event 'reconnecting (hash (wait_seconds wait)))
            (thread-sleep! wait)
            (loop (min (* backoff 2) 30))))))))

(def (ws-receive-loop ws)
  "Receive and process WebSocket messages until disconnect."
  (let loop ()
    (when *ws-running*
      (let ((msg (with-catch
                   (lambda (e) #f)  ; connection closed or error
                   (lambda () (WebSocket-recv ws)))))
        (when msg
          (let ((data (message-data msg)))
            (when (string? data)
              (process-envelope ws data)))
          (loop))))))

;;;; Envelope Processing

(def (process-envelope ws data)
  "Parse a Socket Mode envelope, send ack, and dispatch the event."
  (let ((envelope (with-catch
                    (lambda (e) #f)
                    (lambda ()
                      (parameterize ((read-json-key-as-symbol? #t))
                        (call-with-input-string data read-json))))))
    (when (and envelope (hash-table? envelope))
      (let-hash envelope
        ;; Send acknowledgement
        (when .?envelope_id
          (let ((ack (json-object->string
                      (hash ("envelope_id" .envelope_id)))))
            (WebSocket-send ws (make-message ack 'text #f))))

        ;; Dispatch based on envelope type
        (cond
         ((and .?type (string=? .type "events_api"))
          (dispatch-events-api .?payload))
         ((and .?type (string=? .type "slash_commands"))
          (emit-event 'slash-command .?payload .?envelope_id))
         ((and .?type (string=? .type "interactive"))
          (emit-event 'interactive .?payload .?envelope_id))
         ;; hello message on connect
         ((and .?type (string=? .type "hello"))
          (void))
         ;; disconnect request
         ((and .?type (string=? .type "disconnect"))
          (emit-event 'disconnect-requested .?payload)))))))

(def (dispatch-events-api payload)
  "Extract and dispatch the inner event from an events_api payload."
  (when (and payload (hash-table? payload))
    (let-hash payload
      (when .?event
        (let-hash .event
          (let ((event-type (and .?type (string->symbol
                                         (pregexp-replace* "_" .type "-")))))
            (when event-type
              (emit-event event-type .event))))))))

;;;; Utilities

(def (pregexp-replace* pattern str replacement)
  "Simple string replacement (no regex needed for underscore to dash)."
  (let loop ((chars (string->list str))
             (acc []))
    (if (null? chars)
      (list->string (reverse acc))
      (if (char=? (car chars) #\_)
        (loop (cdr chars) (cons #\- acc))
        (loop (cdr chars) (cons (car chars) acc))))))

(def (socket-mode-connected?)
  "Check if Socket Mode is currently connected."
  (and *ws-running* *ws-connection* #t))
