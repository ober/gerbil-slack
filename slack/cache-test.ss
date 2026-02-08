;; -*- Gerbil -*-
;; Tests for slack/cache

(import
  :std/test
  :std/sugar
  :std/text/json
  :ober/slack/types
  :ober/slack/events
  :ober/slack/cache)

(export cache-test)

(def cache-test
  (test-suite "slack/cache"

    (test-case "cache open and close with in-memory db"
      (cache-open! ":memory:")
      (check (not (not (cache-db))) => #t)
      (cache-close!)
      (check *cache-db* => #f))

    (test-case "user cache round-trip"
      (cache-open! ":memory:")
      (let ((u (make-user id: "U123" name: "alice" real-name: "Alice Smith"
                          display-name: "alice" email: "alice@example.com"
                          avatar-url: #f is-bot: #f is-admin: #f
                          tz: "US/Eastern" status-text: "" status-emoji: ""
                          presence: "active")))
        (cache-user! u)
        (let ((retrieved (cache-get-user "U123")))
          (check (user? retrieved) => #t)
          (check (user-id retrieved) => "U123")
          (check (user-name retrieved) => "alice")
          (check (user-real-name retrieved) => "Alice Smith")))
      (cache-close!))

    (test-case "user cache - not found returns #f"
      (cache-open! ":memory:")
      (check (cache-get-user "NONEXISTENT") => #f)
      (cache-close!))

    (test-case "user cache - update existing"
      (cache-open! ":memory:")
      (let ((u1 (make-user id: "U123" name: "alice" real-name: "Alice"
                           display-name: #f email: #f avatar-url: #f
                           is-bot: #f is-admin: #f tz: #f
                           status-text: #f status-emoji: #f presence: #f)))
        (cache-user! u1)
        (let ((u2 (make-user id: "U123" name: "alice" real-name: "Alice Updated"
                             display-name: #f email: #f avatar-url: #f
                             is-bot: #f is-admin: #f tz: #f
                             status-text: #f status-emoji: #f presence: #f)))
          (cache-user! u2)
          (let ((retrieved (cache-get-user "U123")))
            (check (user-real-name retrieved) => "Alice Updated"))))
      (cache-close!))

    (test-case "cache-all-users"
      (cache-open! ":memory:")
      (cache-user! (make-user id: "U1" name: "alice" real-name: "Alice"
                              display-name: #f email: #f avatar-url: #f
                              is-bot: #f is-admin: #f tz: #f
                              status-text: #f status-emoji: #f presence: #f))
      (cache-user! (make-user id: "U2" name: "bob" real-name: "Bob"
                              display-name: #f email: #f avatar-url: #f
                              is-bot: #f is-admin: #f tz: #f
                              status-text: #f status-emoji: #f presence: #f))
      (let ((users (cache-all-users)))
        (check (length users) => 2)
        (check (user-name (car users)) => "alice")
        (check (user-name (cadr users)) => "bob"))
      (cache-close!))

    (test-case "cache-find-user-by-name"
      (cache-open! ":memory:")
      (cache-user! (make-user id: "U1" name: "alice" real-name: "Alice"
                              display-name: #f email: #f avatar-url: #f
                              is-bot: #f is-admin: #f tz: #f
                              status-text: #f status-emoji: #f presence: #f))
      (let ((found (cache-find-user-by-name "alice")))
        (check (user? found) => #t)
        (check (user-id found) => "U1"))
      (check (cache-find-user-by-name "nobody") => #f)
      (cache-close!))

    (test-case "channel cache round-trip"
      (cache-open! ":memory:")
      (let ((ch (make-channel id: "C123" name: "general" topic: "General chat"
                              purpose: "Main channel" is-channel: #t is-im: #f
                              is-mpim: #f is-private: #f is-archived: #f
                              is-member: #t creator: "U001" created: 1000000
                              num-members: 50)))
        (cache-channel! ch)
        (let ((retrieved (cache-get-channel "C123")))
          (check (channel? retrieved) => #t)
          (check (channel-id retrieved) => "C123")
          (check (channel-name retrieved) => "general")
          (check (channel-topic retrieved) => "General chat")))
      (cache-close!))

    (test-case "cache-all-channels"
      (cache-open! ":memory:")
      (cache-channel! (make-channel id: "C1" name: "alpha" topic: #f purpose: #f
                                    is-channel: #t is-im: #f is-mpim: #f
                                    is-private: #f is-archived: #f is-member: #t
                                    creator: #f created: #f num-members: #f))
      (cache-channel! (make-channel id: "C2" name: "beta" topic: #f purpose: #f
                                    is-channel: #t is-im: #f is-mpim: #f
                                    is-private: #f is-archived: #f is-member: #t
                                    creator: #f created: #f num-members: #f))
      (let ((channels (cache-all-channels)))
        (check (length channels) => 2))
      (cache-close!))

    (test-case "cache-find-channel-by-name"
      (cache-open! ":memory:")
      (cache-channel! (make-channel id: "C1" name: "general" topic: #f purpose: #f
                                    is-channel: #t is-im: #f is-mpim: #f
                                    is-private: #f is-archived: #f is-member: #t
                                    creator: #f created: #f num-members: #f))
      (let ((found (cache-find-channel-by-name "general")))
        (check (channel? found) => #t)
        (check (channel-id found) => "C1"))
      (check (cache-find-channel-by-name "nope") => #f)
      (cache-close!))

    (test-case "message cache round-trip"
      (cache-open! ":memory:")
      (let ((msg (make-message ts: "1234567890.123456" channel: "C123"
                               user: "U456" text: "Hello world"
                               thread-ts: #f reply-count: 0
                               reactions: [] attachments: [] files: []
                               edited: #f is-bot: #f bot-id: #f)))
        (cache-message! msg)
        (let ((msgs (cache-get-messages "C123")))
          (check (length msgs) => 1)
          (let ((retrieved (car msgs)))
            (check (message-ts retrieved) => "1234567890.123456")
            (check (message-text retrieved) => "Hello world")
            (check (message-user retrieved) => "U456"))))
      (cache-close!))

    (test-case "cache-get-messages with limit and before"
      (cache-open! ":memory:")
      ;; Insert 5 messages
      (for-each
        (lambda (i)
          (cache-message! (make-message ts: (string-append "100000000" (number->string i) ".000000")
                                        channel: "C1" user: "U1"
                                        text: (string-append "msg " (number->string i))
                                        thread-ts: #f reply-count: 0
                                        reactions: [] attachments: [] files: []
                                        edited: #f is-bot: #f bot-id: #f)))
        '(1 2 3 4 5))
      ;; Get with limit
      (let ((msgs (cache-get-messages "C1" 3)))
        (check (length msgs) => 3))
      ;; Get with before
      (let ((msgs (cache-get-messages "C1" 10 "1000000003.000000")))
        (check (length msgs) => 2))
      (cache-close!))

    (test-case "cache-get-thread"
      (cache-open! ":memory:")
      (let ((parent-ts "1000000001.000000"))
        ;; Parent message
        (cache-message! (make-message ts: parent-ts channel: "C1" user: "U1"
                                      text: "Thread start" thread-ts: #f
                                      reply-count: 2 reactions: [] attachments: []
                                      files: [] edited: #f is-bot: #f bot-id: #f))
        ;; Replies
        (cache-message! (make-message ts: "1000000002.000000" channel: "C1" user: "U2"
                                      text: "Reply 1" thread-ts: parent-ts
                                      reply-count: 0 reactions: [] attachments: []
                                      files: [] edited: #f is-bot: #f bot-id: #f))
        (cache-message! (make-message ts: "1000000003.000000" channel: "C1" user: "U1"
                                      text: "Reply 2" thread-ts: parent-ts
                                      reply-count: 0 reactions: [] attachments: []
                                      files: [] edited: #f is-bot: #f bot-id: #f))
        (let ((thread (cache-get-thread "C1" parent-ts)))
          (check (length thread) => 3)
          (check (message-text (car thread)) => "Thread start")))
      (cache-close!))

    (test-case "cache-delete-message!"
      (cache-open! ":memory:")
      (cache-message! (make-message ts: "1000000001.000000" channel: "C1" user: "U1"
                                    text: "To be deleted" thread-ts: #f
                                    reply-count: 0 reactions: [] attachments: []
                                    files: [] edited: #f is-bot: #f bot-id: #f))
      (check (length (cache-get-messages "C1")) => 1)
      (cache-delete-message! "C1" "1000000001.000000")
      (check (length (cache-get-messages "C1")) => 0)
      (cache-close!))

    (test-case "metadata key-value store"
      (cache-open! ":memory:")
      (cache-set-meta! "test_key" "test_value")
      (check (cache-get-meta "test_key") => "test_value")
      (check (cache-get-meta "nonexistent") => #f)
      ;; Update
      (cache-set-meta! "test_key" "updated_value")
      (check (cache-get-meta "test_key") => "updated_value")
      (cache-close!))

    (test-case "TTL staleness checks"
      (cache-open! ":memory:")
      ;; Initially stale (no metadata)
      (check (cache-users-stale?) => #t)
      (check (cache-channels-stale?) => #t)
      ;; Mark fresh
      (cache-mark-users-fresh!)
      (cache-mark-channels-fresh!)
      (check (cache-users-stale?) => #f)
      (check (cache-channels-stale?) => #f)
      (cache-close!))

    (test-case "cache-stats"
      (cache-open! ":memory:")
      (let ((stats (cache-stats)))
        (check (hash-ref stats "users") => 0)
        (check (hash-ref stats "channels") => 0)
        (check (hash-ref stats "messages") => 0))
      (cache-user! (make-user id: "U1" name: "a" real-name: #f display-name: #f
                              email: #f avatar-url: #f is-bot: #f is-admin: #f
                              tz: #f status-text: #f status-emoji: #f presence: #f))
      (let ((stats (cache-stats)))
        (check (hash-ref stats "users") => 1))
      (cache-close!))

    (test-case "event-driven message cache update"
      (cache-open! ":memory:")
      ;; Emit a message event — should be cached automatically
      (emit-event 'message
        (hash (ts "9999.000000")
              (channel "C999")
              (user "U999")
              (text "Event-driven message")))
      (let ((msgs (cache-get-messages "C999")))
        (check (length msgs) => 1)
        (check (message-text (car msgs)) => "Event-driven message"))
      ;; Emit delete event
      (emit-event 'message-deleted
        (hash (channel "C999")
              (deleted_ts "9999.000000")))
      (check (length (cache-get-messages "C999")) => 0)
      (cache-close!)
      (clear-handlers!))))
