;; -*- Gerbil -*-
;; Tests for slack/events

(import
  :std/test
  :std/sugar
  :ober/slack/events)

(export events-test)

(def events-test
  (test-suite "slack/events"

    (test-case "slack-event construction"
      (let ((ev (make-slack-event type: 'message data: (hash (text "hello")) envelope-id: "abc")))
        (check (slack-event? ev) => #t)
        (check (slack-event-type ev) => 'message)
        (check (slack-event-envelope-id ev) => "abc")
        (check (hash-get (slack-event-data ev) 'text) => "hello")))

    (test-case "on-event and emit-event"
      (clear-handlers!)
      (let ((received #f))
        (on-event 'message (lambda (ev) (set! received ev)))
        (emit-event 'message (hash (text "test")))
        (check (slack-event? received) => #t)
        (check (slack-event-type received) => 'message)
        (check (hash-get (slack-event-data received) 'text) => "test"))
      (clear-handlers!))

    (test-case "multiple handlers for same type"
      (clear-handlers!)
      (let ((count 0))
        (on-event 'message (lambda (ev) (set! count (+ count 1))))
        (on-event 'message (lambda (ev) (set! count (+ count 10))))
        (emit-event 'message #f)
        (check count => 11))
      (clear-handlers!))

    (test-case "catch-all handler with '*"
      (clear-handlers!)
      (let ((events []))
        (on-event '* (lambda (ev) (set! events (cons (slack-event-type ev) events))))
        (emit-event 'message #f)
        (emit-event 'reaction-added #f)
        (check (length events) => 2)
        (check (car events) => 'reaction-added)
        (check (cadr events) => 'message))
      (clear-handlers!))

    (test-case "remove-handler"
      (clear-handlers!)
      (let ((count 0))
        (let ((id (on-event 'message (lambda (ev) (set! count (+ count 1))))))
          (emit-event 'message #f)
          (check count => 1)
          (remove-handler 'message id)
          (emit-event 'message #f)
          (check count => 1)))  ;; still 1, handler was removed
      (clear-handlers!))

    (test-case "handler-count"
      (clear-handlers!)
      (check (handler-count) => 0)
      (on-event 'message (lambda (ev) #f))
      (on-event 'message (lambda (ev) #f))
      (on-event 'reaction-added (lambda (ev) #f))
      (check (handler-count 'message) => 2)
      (check (handler-count 'reaction-added) => 1)
      (check (handler-count) => 3)
      (clear-handlers!))

    (test-case "clear-handlers!"
      (clear-handlers!)
      (on-event 'message (lambda (ev) #f))
      (on-event 'reaction-added (lambda (ev) #f))
      (check (handler-count) => 2)
      (clear-handlers!)
      (check (handler-count) => 0))))
