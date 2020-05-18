;; -*- Gerbil -*-
;; ©ober 2020
;; slack client library

(import
  :gerbil/gambit
  :gerbil/gambit/bits
  :gerbil/gambit/misc
  :gerbil/gambit/os
  :gerbil/gambit/ports
  :gerbil/gambit/threads
  :std/actor/message
  :std/actor/proto
  :std/coroutine
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/error
  :std/format
  :std/generic
  :std/iter
  :std/logger
  :std/misc/completion
  :std/misc/list
  :std/misc/threads
  :std/net/request
  :std/net/websocket
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :ober/oberlib)

(export #t)
(declare (not optimize-dead-definitions))

(def version "0.05")

(def program-name "slack")
(def config-file "~/.slack.yaml")

(def user-list (hash))
(def channel-list (hash))

(def (get-chat-list)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/im.list?token=~a" .token))
      (with ([status . body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            .?ims))))))

(def (chats)
  (let ((chats (get-chat-list))
        (outs [[ "user" "id" "user-id" "priority" "is_user_deleted" "is_archived" "is_im" "created" "is_org_shared" ]]))
    (for (chat chats)
      (let-hash chat
        (set! outs (cons [
                          (id-for-user .?user)
                          .?id
                          .?user
                          .?priority
                          .?is_user_deleted
                          .?is_archived
                          .?is_im
                          .?created
                          .?is_org_shared
                          ] outs))))
    (style-output outs)))

(def (ghistory group)
  (let-hash (load-config)
    (let ((url (format "https://slack.com/api/groups.history?token=~a&channel=~a&count=~a" .token group 2000))
          (outs [[ "User" "Message" "ts" "team" ]]))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (when .?messages
              (for (message .messages)
                (dp (table->list message))
                (let-hash message
                  (set! outs (cons [
                                    (if .?user
                                      (user-from-id .?user users-hash)
                                      .?user)
                                    .?text
                                    .?ts
                                    .?team ] outs))))))))
      (style-output outs))))

(def (user user)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/users.info?token=~a&user=~a" .token user))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (present-item body)))))




(def (groups)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/groups.list?token=~a" .token))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (displayln "|name|creator|is_group|purpose|members|created|name2|id |is_archived|is_mpim|topic|priority|")
            (displayln "|--|-------|-----|-------------|----|---------------|--------|")
            (for (g .groups)
              (let-hash g
                (displayln "|" .name_normalized
                           "|" (hash-ref .topic 'value)
                           "|" .creator
                           "|" .is_group
                           "|" (hash-ref .purpose 'value)
                           "|" .members
                           "|" .created
                           "|" .name
                           "|" .id
                           "|" .is_archived
                           "|" .is_mpim
                           "|" .priority "|")))))))))

(def (post channel message from)
  (let-hash (load-config)
    (let* ((url "https://slack.com/api/chat.postMessage")
           (msg (get-if-set-b64 "slackmsg" message))
	   (data (json-object->string
		  (hash
		   ("as_user" #t)
		   ("username" from)
		   ("text" msg)
		   ("channel" channel)))))
      (with ([ status body ] (rest-call 'post url (default-headers) data))
        (unless status
          (error body))
        (present-item body)))))

(def (im-open name)
  (let* ((url "https://slack.com/api/im.open")
	 (id (id-for-user name))
	 (data (json-object->string
		(hash
		 ("user" id)
		 ("return_im" #t)))))
    (with ([status body] (rest-call 'post url (default-headers) data))
      (unless status
        (error body))
      (when (table? body)
        (let-hash body
          (when .?channel
            (let-hash .channel
              .id)))))))

(def (msg user message)
  (let-hash (load-config)
    (let ((channel (im-open user))
          (msg (get-if-set-b64 "slackmsg" message)))
      (if channel
	(displayln (post channel msg .username))
	(displayln "Invalid user: " user)))))

(def (emojis)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/emoji.list?token=~a" .token))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (present-item body)))))

(def (delete channel timestamp)
  "Remove a message from a chat channel"
  (let-hash (load-config)
    (let* ((url "https://slack.com/api/chat.delete")
	   (data (json-object->string
		  (hash
		   ("as_user" #t)
		   ("ts" timestamp)
		   ("channel" channel)))))
      (with ([status body] (rest-call 'post url (default-headers) data))
        (unless status
          (error body))
        (present-item body)))))

(def (search query)
  "Search all conversations for query"
  (let-hash (load-config)
    (let ((url (format "https://slack.com/api/search.messages?token=~a&count=~a&query=~a" .token 100 query))
          (outs [[ "id" "channel" "Message" "channel-id" "ts" "type" "user" "permalink" ]]))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (if .?messages
              (let-hash .?messages
                (if .?matches
                  (let-hash .?matches
                    (for (m ..matches)
                      (let-hash m
                        (set! outs (cons [ (or .?username "Unknown")
                                           (hash-get .channel 'name)
                                           (or .?text "No Text")
                                           (hash-get .channel 'id)
                                           .?ts
                                           .?type
                                           .?user
                                           .?permalink ] outs)))))))))))
      (style-output outs))))

(def (get-group-list)
  ;;  (cache-or-run "~/.slack-groups.cache" 2592000
  (let-hash (load-config)
    (let ((url (format "https://slack.com/api/groups.list?token=~a" .token)))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            .?groups))))))

(def (gul)
  (for (group (get-group-list))
    (present-item group)))

(def (get-user-list)
  ;;(cache-or-run "~/.slack-users.cache" 2592000
  (let-hash (load-config)
    (let ((url (format "https://slack.com/api/users.list?token=~a" .?token)))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (pi "boom")
          (error body))
        (when (table? body)
          (let-hash body
            .?members))))))

(def (users)
  (let* ((members (get-user-list))
         (outs [[ "name" "real_name" "is_primary_owner" "is_ultra_restricted" "is_restricted" "team_id" "updated" "is_app_user" "profile" "id" "tz_label" ]]))
    (for (user members)
      (let-hash user
        (set! outs (cons [
                          .?name
                          .?real_name
                          .?is_primary_owner
                          .?is_ultra_restricted
                          .?is_restricted
                          .?team_id
                          .?updated
                          .?is_app_user
                          (hash->list .profile)
                          .?id
                          .?tz_label ] outs))))
    (style-output outs)))

(def (id-for-user username)
  (let ((id #f)
        (users (get-user-list)))
    (for (user users)
      (let-hash user
        (when (string=? .name username)
          (set! id .id))))
    id))

(def (id-for-channel channel)
  (let ((id #f))
    (let-hash (get-channel-list)
      (for (c .channels)
        (let-hash c
          (when (string=? .name channel)
            (set! id .id)))))
    id))

(def (get-channel-list)
  ;;(cache-or-run "~/.slack-channels.cache" 2592000
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/channels.list?token=~a" .token))
      (with ([status body] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          body)))))

(def (channels)
  (let-hash (get-channel-list)
    (print-channels .channels)))

(def (users-hash)
  (let ((users (hash))
        (members (get-user-list)))
    (for (member members)
      (let-hash member
        (hash-put! users (string->symbol .id) .name)))
    users))

(def (user-from-id id users-hash)
  (if id
    (hash-get users-hash (string->symbol id))
    id))

(def (im-history user)
  (let-hash (load-config)
    (let ((outs [[ "User" "Datetime" "Message" "channel-id" "ts" "team" ]])
          (members (users-hash))
          (ts 0))
      (let lp ((latest ts))
        (let (url (format "https://slack.com/api/im.history?token=~a&channel=~a&count=1000&latest=~a" .token user latest))
          (with ([ status body ] (rest-call 'get url (default-headers)))
            (unless status
              (error body))
            (when (table? body)
              (let-hash body
                (when .?messages
                  (for (message .messages)
                    (let-hash message
                      (set! outs (cons [
                                        (user-from-id .?user members)
                                        (print-date (epoch->date (float->int (string->number .?ts))))
                                        .?text
                                        user
                                        .?ts
                                        .?team ] outs))
                      (set! ts .?ts))))
                (displayln (format "more: ~a latest: ~a" .?has_more .?latest))
                (when .?has_more
                  (lp ts)))))))
      (style-output outs))))

(def (channel-history channel)
  (let-hash (load-config)
    (let* ((users-hash (users-hash))
           (id (id-for-channel channel))
           (url (format "https://slack.com/api/channels.history?token=~a&channel=~a&count=1000" .token id))
           (outs [[ "User" "Channel" "Message" "channel-id" "ts" "team" ]]))
      (with ([ status body ] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (when .?messages
              (for (message .messages)
                (dp (table->list message))
                (let-hash message
                  (set! outs (cons [ (user-from-id .?user users-hash)
                                     channel
                                     .?text
                                     id
                                     .?ts
                                     .?team ] outs))))))))
      (style-output outs))))

(def (print-channels channels)
  (when (list? channels)
    (for (channel channels)
      (print-channel channel))))

(def (print-channel channel)
  (if (table? channel)
    (let-hash channel
      (displayln "is_channel: " .?is_channel
                 " created: " .?created
                 " is_member: " .?is_member
                 " is_mpim: " .?is_mpim
                 " previous_names: " .?previous_names
                 " id: " .?id
                 " unlinked: " .?unlinked
                 " priority: " .?priority
                 " has_pins: " .?has_pins
                 " name: " .?name
                 " is_archived: " .?is_archived
                 " is_general: " .?is_general
                 " creator: " .?creator
                 " name_normalized: " .?name_normalized
                 " is_org_shared: " .?is_org_shared
                 " is_private: " .?is_private))))

(def (set-topic channel topic)
  (let* ((url "https://slack.com/api/channels.setTopic")
         (data (json-object->string
                (hash
                 ("topic" topic)
                 ("channel" channel)))))
    (with ([status body] (rest-call 'post url (default-headers) data))
      (unless status
        (error body))
      (when (table? body)
        (let-hash body
          (present-item body))))))

(def (load-config)
  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (table? (car config-data)))
      (displayln (format "Could not parse your config ~a" config-file))
      (exit 2))

    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car config-data))

    (let-hash config
      (when (and .?key .?iv .?password)
        (hash-put! config 'token (get-password-from-config .key .iv .password)))
      (hash-put! config 'style (or .?style "org-mode"))
      (when .?secrets
        (let-hash (u8vector->object (base64-decode .secrets))
          (let ((password (get-password-from-config .key .iv .password)))
            (hash-put! config 'token password)
            config))))))

(def (load-config-old)
  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (table? (car config-data)))
      (displayln (format "Could not parse your config ~a" config-file))
      (exit 2))

    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car config-data))

    (let-hash config
      (when (and .?key .?iv .?password)
        (displayln "token is " (get-password-from-config .key .iv .password))
        (hash-put! config 'token (get-password-from-config .key .iv .password)))
      config)))

(def (default-headers)
  (let-hash (load-config)
    [
     ["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]
     ]))

(def (whisper user channel msg)
  (let-hash (load-config)
    (let* ((url "https://slack.com/api/chat.postEphemeral")
           (data (json-object->string
                  (hash
                   ("as_user" #t)
                   ("attachments" #f)
                   ("channel" channel)
                   ("id" (im-open user))
                   ("link_names" #t)
                   ("parse" "none")
                   ("user" (id-for-user user))
                   ("text" msg)))))
      (with ([ status body ] (rest-call 'post url (default-headers) data))
        (unless status
          (error body))
        (present-item body)))))

(def (config)
  (let-hash (load-config)
    (displayln "Please enter your slack token: ")
    (let* ((password (read-password ##console-port))
           (cipher (make-aes-256-ctr-cipher))
           (iv (random-bytes (cipher-iv-length cipher)))
           (key (random-bytes (cipher-key-length cipher)))
           (encrypted-password (encrypt cipher key iv password))
           (enc-pass-store (u8vector->base64-string encrypted-password))
           (iv-store (u8vector->base64-string iv))
           (key-store (u8vector->base64-string key))
           (secrets (base64-encode (object->u8vector
                                    (hash
                                     (password enc-pass-store)
                                     (iv iv-store)
                                     (key key-store))))))
      (displayln "Add the following secrets line to your " config-file)
      (displayln "")
      (displayln "secrets: " secrets))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (rtm-start-json)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/rtm.start?token=~a" .token))
      (with ([ status body ] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (present-item body))))))

(def (rtm-start)
  (let-hash (load-config)
    (let (url (format "https://slack.com/api/rtm.start?token=~a" .token))
      (with ([ status body ] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (displayln "self: " (hash->list .self))
            (displayln "OK: " .ok)
            (displayln "dnd: " (if (and .?dnd (table? .?dnd)) (hash->list .dnd)))
            (displayln "url: " .?url)
            (displayln "Channels: ------------------------------------------------------------")
            (print-channels .?channels)
            (displayln "Users: ------------------------------------------------------------")
            ;;(print-users .?users)
            (for (user .?users)
              (displayln (hash->list user)))

            (displayln "Bots: ------------------------------------------------------------")
            (for (bot .?bots)
              (displayln (hash->list bot)))

            (displayln "team: " (hash->list .?team))
            (displayln "groups: " .?groups)
            (displayln "self: " .?self)
            (displayln "ims: " .?ims)
            (displayln "thread_only_channels: " .?thread_only_channels)
            (displayln "non_threadable_channels: " .?non_threadable_channels)
            (displayln "subteams: " .?subteams)
            (displayln "cache_version: " .?cache_version)
            (displayln "cache_ts_version: " .?cache_ts_version)
            (displayln "latest_event_ts: " .?latest_event_ts)
            (displayln "can_manage_shared_channels: " .?can_manage_shared_channels)
            (displayln "cache_ts: " .?cache_ts)
            (displayln "read_only_channels: " .?read_only_channels)))))))

(def (slack-send ws msg)
  (let (outp (open-output-u8vector))
    (write-json msg outp)
    (websocket-send ws (get-output-u8vector outp) 'text)))

(def (slack-recv ws)
  (let ((values bytes type) (websocket-recv ws))
    (if (eq? type 'text)
      (read-json (open-input-u8vector [char-encoding: 'UTF-8 init: bytes]))
      (begin
        (warning "wamp-recv: server sent binary data (~s)" (u8vector-length bytes))
        (raise-io-error 'wamp-recv "server sent binary data" bytes)))))

(def (gw group channel message)
  "Take a group and whisper the message to each member in the channel"
  (let-hash (load-config)
    (when .?groups
      (when (table? .groups)
        (if (hash-ref .groups group)
          (for (user (hash-ref .groups group))
            (whisper user channel message))
          (displayln "Error: group" group " not found in " .groups))))))

(def (set-presence status)
  "Set your status to Active, or Away"
  (unless (or (string=? status "active")
              (string=? status "away"))
    (error "Error: only away, or active is allowed for status"))
  (let-hash (load-config)
    (let ((url "https://slack.com/api/users.setPresence")
          (data (json-object->string (hash ("presence" status)))))
      (with ([ status body ] (rest-call 'post url (default-headers) data))
        (unless status
          (error body))
        (present-item body)))))

(def (presence user)
  "Get the presence status. away, or active of a user"
  (let-hash (load-config)
    (let* ((id (id-for-user user))
           (url (format "https://slack.com/api/users.getPresence?user=~a&token=~a" id .token)))
      (with ([ status body ] (rest-call 'get url (default-headers)))
        (unless status
          (error body))
        (when (table? body)
          (let-hash body
            (displayln (format "~a is currently ~a" user .presence))))))))

(def (away)
  "Set status away"
  (set-presence "away"))

(def (active)
  "Set status active"
  (set-presence "active"))
