;; -*- Gerbil -*-
;; Event types, dispatcher, and subscriptions

(import
  :std/sugar
  :std/iter)

(export #t)

;;;; Event Type

(defclass slack-event (type data envelope-id)
  transparent: #t)

;;;; Event Type Symbols
;; These are the known event types dispatched by the system

;; Messages
;; 'message 'message-changed 'message-deleted

;; Reactions
;; 'reaction-added 'reaction-removed

;; Channels
;; 'channel-created 'channel-archived 'channel-unarchive

;; Members
;; 'member-joined-channel 'member-left-channel

;; Presence & typing
;; 'user-typing 'presence-change

;; Team
;; 'team-join

;; Emoji
;; 'emoji-changed

;; Files
;; 'file-shared 'file-deleted

;; Pins
;; 'pin-added 'pin-removed

;; Status
;; 'user-status-changed

;; Connection
;; 'connected 'disconnected 'reconnecting

;;;; Event Dispatcher

;; Global handler registry: hash of event-type -> list of (id . handler)
(def *event-handlers* (make-hash-table))
(def *handler-counter* 0)

(def (on-event type handler)
  "Register a handler for an event type. Returns handler-id for removal."
  (set! *handler-counter* (+ *handler-counter* 1))
  (let ((id *handler-counter*)
        (existing (or (hash-get *event-handlers* type) [])))
    (hash-put! *event-handlers* type (cons (cons id handler) existing))
    id))

(def (remove-handler type handler-id)
  "Remove a handler by its ID."
  (let ((handlers (or (hash-get *event-handlers* type) [])))
    (hash-put! *event-handlers* type
               (filter (lambda (pair) (not (= (car pair) handler-id)))
                       handlers))))

(def (emit-event type data (envelope-id #f))
  "Dispatch an event to all registered handlers for its type.
   Also dispatches to handlers registered for '* (catch-all)."
  (let ((event (make-slack-event type: type data: data envelope-id: envelope-id)))
    ;; Type-specific handlers
    (let ((handlers (or (hash-get *event-handlers* type) [])))
      (for-each (lambda (pair) ((cdr pair) event)) handlers))
    ;; Catch-all handlers
    (let ((all-handlers (or (hash-get *event-handlers* '*) [])))
      (for-each (lambda (pair) ((cdr pair) event)) all-handlers))
    event))

(def (clear-handlers!)
  "Remove all event handlers."
  (set! *event-handlers* (make-hash-table))
  (set! *handler-counter* 0))

(def (handler-count (type #f))
  "Count registered handlers. If type given, count for that type only."
  (if type
    (length (or (hash-get *event-handlers* type) []))
    (hash-fold (lambda (k v acc) (+ acc (length v))) 0 *event-handlers*)))
