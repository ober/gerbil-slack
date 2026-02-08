;; -*- Gerbil -*-
;; Slack conversations API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (conversations-list types: (types "public_channel,private_channel,mpim,im")
                         limit: (limit 200)
                         token: (token #f))
  "List conversations. types: comma-separated list. Returns list of channel structs."
  (let ((channels (slack-api-paginated "conversations.list"
                                        (list (cons "types" types))
                                        key: 'channels
                                        limit: limit
                                        token: token)))
    (map json->channel channels)))

(def (conversations-info channel-id token: (token #f))
  "Get info about a conversation. Returns channel struct."
  (let* ((resp (slack-api "conversations.info"
                          (list (cons "channel" channel-id))
                          token: token))
         (ch (hash-get resp 'channel)))
    (if ch (json->channel ch) (error "No channel in response"))))

(def (conversations-history channel-id
                            limit: (limit 100)
                            latest: (latest #f)
                            oldest: (oldest #f)
                            token: (token #f))
  "Get message history for a conversation. Returns list of message structs."
  (let* ((params (append (list (cons "channel" channel-id)
                               (cons "limit" (number->string limit)))
                         (if latest (list (cons "latest" latest)) [])
                         (if oldest (list (cons "oldest" oldest)) [])))
         (resp (slack-api "conversations.history" params token: token))
         (messages (hash-get resp 'messages)))
    (if messages (map json->message messages) [])))

(def (conversations-replies channel-id ts
                            limit: (limit 100)
                            token: (token #f))
  "Get thread replies. Returns list of message structs (includes parent)."
  (let* ((params (list (cons "channel" channel-id)
                       (cons "ts" ts)
                       (cons "limit" (number->string limit))))
         (resp (slack-api "conversations.replies" params token: token))
         (messages (hash-get resp 'messages)))
    (if messages (map json->message messages) [])))

(def (conversations-members channel-id
                            limit: (limit 200)
                            token: (token #f))
  "List members of a conversation. Returns list of user-id strings (paginated)."
  (slack-api-paginated "conversations.members"
                       (list (cons "channel" channel-id))
                       key: 'members
                       limit: limit
                       token: token))

(def (conversations-open users: (users #f)
                         channel: (channel #f)
                         token: (token #f))
  "Open or resume a conversation. Returns channel-id string."
  (let* ((params (append (if users (list (cons "users" (if (list? users)
                                                        (string-join users ",")
                                                        users))) [])
                         (if channel (list (cons "channel" channel)) [])
                         (list (cons "return_im" "true"))))
         (resp (slack-post "conversations.open" params token: token))
         (ch (hash-get resp 'channel)))
    (if (hash-table? ch)
      (hash-get ch 'id)
      ch)))

(def (conversations-create name
                           is-private: (is-private #f)
                           token: (token #f))
  "Create a new conversation. Returns channel struct."
  (let* ((params (append (list (cons "name" name))
                         (if is-private (list (cons "is_private" "true")) [])))
         (resp (slack-post "conversations.create" params token: token))
         (ch (hash-get resp 'channel)))
    (if ch (json->channel ch) (error "No channel in response"))))

(def (conversations-set-topic channel-id topic token: (token #f))
  "Set conversation topic."
  (slack-post "conversations.setTopic"
              (list (cons "channel" channel-id)
                    (cons "topic" topic))
              token: token)
  #t)

(def (conversations-set-purpose channel-id purpose token: (token #f))
  "Set conversation purpose."
  (slack-post "conversations.setPurpose"
              (list (cons "channel" channel-id)
                    (cons "purpose" purpose))
              token: token)
  #t)

(def (conversations-join channel-id token: (token #f))
  "Join a conversation."
  (slack-post "conversations.join"
              (list (cons "channel" channel-id))
              token: token)
  #t)

(def (conversations-leave channel-id token: (token #f))
  "Leave a conversation."
  (slack-post "conversations.leave"
              (list (cons "channel" channel-id))
              token: token)
  #t)

(def (conversations-invite channel-id users token: (token #f))
  "Invite users to a conversation. users: comma-separated user IDs or list."
  (let ((user-str (if (list? users) (string-join users ",") users)))
    (slack-post "conversations.invite"
                (list (cons "channel" channel-id)
                      (cons "users" user-str))
                token: token)
    #t))

(def (conversations-kick channel-id user-id token: (token #f))
  "Remove a user from a conversation."
  (slack-post "conversations.kick"
              (list (cons "channel" channel-id)
                    (cons "user" user-id))
              token: token)
  #t)

(def (conversations-archive channel-id token: (token #f))
  "Archive a conversation."
  (slack-post "conversations.archive"
              (list (cons "channel" channel-id))
              token: token)
  #t)

(def (conversations-mark channel-id ts token: (token #f))
  "Mark a conversation as read up to given timestamp."
  (slack-post "conversations.mark"
              (list (cons "channel" channel-id)
                    (cons "ts" ts))
              token: token)
  #t)
