;; -*- Gerbil -*-
;; Tests for slack/types

(import
  :std/test
  :std/sugar
  :std/text/json
  :ober/slack/types)

(export types-test)

(def types-test
  (test-suite "slack/types"

    (test-case "user struct construction"
      (let ((u (make-user id: "U123" name: "alice" real-name: "Alice Smith")))
        (check (user? u) => #t)
        (check (user-id u) => "U123")
        (check (user-name u) => "alice")
        (check (user-real-name u) => "Alice Smith")))

    (test-case "channel struct construction"
      (let ((ch (make-channel id: "C123" name: "general" is-channel: #t)))
        (check (channel? ch) => #t)
        (check (channel-id ch) => "C123")
        (check (channel-name ch) => "general")
        (check (channel-is-channel ch) => #t)))

    (test-case "message struct construction"
      (let ((msg (make-message ts: "1234567890.123456" user: "U123" text: "hello")))
        (check (message? msg) => #t)
        (check (message-ts msg) => "1234567890.123456")
        (check (message-text msg) => "hello")))

    (test-case "json->user parsing"
      (let* ((json-str "{\"id\":\"U123\",\"name\":\"alice\",\"real_name\":\"Alice Smith\",\"is_bot\":false,\"is_admin\":true,\"tz\":\"America/New_York\",\"profile\":{\"display_name\":\"alice.s\",\"email\":\"alice@example.com\",\"image_72\":\"https://img.example.com/72.jpg\",\"status_text\":\"Working\",\"status_emoji\":\":computer:\"}}")
             (h (parameterize ((read-json-key-as-symbol? #t))
                  (call-with-input-string json-str read-json)))
             (u (json->user h)))
        (check (user-id u) => "U123")
        (check (user-name u) => "alice")
        (check (user-real-name u) => "Alice Smith")
        (check (user-display-name u) => "alice.s")
        (check (user-email u) => "alice@example.com")
        (check (user-is-bot u) => #f)
        (check (user-is-admin u) => #t)
        (check (user-status-text u) => "Working")))

    (test-case "json->channel parsing"
      (let* ((json-str "{\"id\":\"C456\",\"name\":\"engineering\",\"is_channel\":true,\"is_private\":false,\"is_archived\":false,\"is_member\":true,\"creator\":\"U001\",\"created\":1600000000,\"num_members\":42,\"topic\":{\"value\":\"Engineering discussion\"},\"purpose\":{\"value\":\"Talk about code\"}}")
             (h (parameterize ((read-json-key-as-symbol? #t))
                  (call-with-input-string json-str read-json)))
             (ch (json->channel h)))
        (check (channel-id ch) => "C456")
        (check (channel-name ch) => "engineering")
        (check (channel-topic ch) => "Engineering discussion")
        (check (channel-purpose ch) => "Talk about code")
        (check (channel-is-channel ch) => #t)
        (check (channel-num-members ch) => 42)))

    (test-case "json->message parsing"
      (let* ((json-str "{\"ts\":\"1234567890.000001\",\"user\":\"U123\",\"text\":\"Hello world\",\"thread_ts\":\"1234567890.000000\",\"reply_count\":3}")
             (h (parameterize ((read-json-key-as-symbol? #t))
                  (call-with-input-string json-str read-json)))
             (msg (json->message h)))
        (check (message-ts msg) => "1234567890.000001")
        (check (message-user msg) => "U123")
        (check (message-text msg) => "Hello world")
        (check (message-thread-ts msg) => "1234567890.000000")
        (check (message-reply-count msg) => 3)))

    (test-case "json->reaction parsing"
      (let* ((json-str "{\"name\":\"thumbsup\",\"count\":5,\"users\":[\"U1\",\"U2\"]}")
             (h (parameterize ((read-json-key-as-symbol? #t))
                  (call-with-input-string json-str read-json)))
             (r (json->reaction h)))
        (check (reaction-name r) => "thumbsup")
        (check (reaction-count r) => 5)
        (check (reaction-users r) => '("U1" "U2"))))

    (test-case "json->team parsing"
      (let* ((json-str "{\"id\":\"T123\",\"name\":\"My Team\",\"domain\":\"myteam\",\"icon\":{\"image_132\":\"https://img.example.com/team.png\"}}")
             (h (parameterize ((read-json-key-as-symbol? #t))
                  (call-with-input-string json-str read-json)))
             (t (json->team h)))
        (check (team-id t) => "T123")
        (check (team-name t) => "My Team")
        (check (team-domain t) => "myteam")
        (check (team-icon-url t) => "https://img.example.com/team.png")))

    (test-case "display helpers"
      (let ((u (make-user id: "U1" name: "alice" display-name: "Alice S" real-name: "Alice Smith"))
            (ch (make-channel id: "C1" name: "general")))
        (check (user-display u) => "Alice S")
        (check (channel-display ch) => "general")))

    (test-case "message-summary truncation"
      (let ((short (make-message text: "short"))
            (long (make-message text: (make-string 100 #\x))))
        (check (message-summary short) => "short")
        (check (< (string-length (message-summary long)) 84) => #t)))))
