;; -*- Gerbil -*-
;; Domain types for Slack API

(import
  :std/sugar
  :std/text/json)

(export #t)

;;;; Domain Types (defclass for keyword construction)

(defclass user
  (id name real-name display-name email avatar-url
   is-bot is-admin tz status-text status-emoji presence)
  transparent: #t)

(defclass channel
  (id name topic purpose is-channel is-im is-mpim
   is-private is-archived is-member creator created num-members)
  transparent: #t)

(defclass message
  (ts channel user text thread-ts reply-count
   reactions attachments files edited is-bot bot-id)
  transparent: #t)

(defclass reaction
  (name count users)
  transparent: #t)

(defclass file-info
  (id name title mimetype url-private size user created)
  transparent: #t)

(defclass team
  (id name domain icon-url)
  transparent: #t)

(defclass reminder
  (id creator user text complete-ts)
  transparent: #t)

(defclass bookmark
  (id channel-id title type link emoji)
  transparent: #t)

;;;; JSON -> Struct Parsers
;; All parsers expect a hash-table with symbol keys (read-json-key-as-symbol? #t)

(def (json->user h)
  (let-hash h
    (let ((profile .?profile))
      (make-user
       id: .?id
       name: .?name
       real-name: (or .?real_name
                      (and profile (hash-table? profile)
                           (hash-get profile 'real_name)))
       display-name: (and profile (hash-table? profile)
                          (hash-get profile 'display_name))
       email: (and profile (hash-table? profile)
                   (hash-get profile 'email))
       avatar-url: (and profile (hash-table? profile)
                        (hash-get profile 'image_72))
       is-bot: .?is_bot
       is-admin: .?is_admin
       tz: .?tz
       status-text: (and profile (hash-table? profile)
                         (hash-get profile 'status_text))
       status-emoji: (and profile (hash-table? profile)
                          (hash-get profile 'status_emoji))
       presence: .?presence))))

(def (json->channel h)
  (let-hash h
    (make-channel
     id: .?id
     name: .?name
     topic: (and .?topic (if (hash-table? .topic)
                           (hash-get .topic 'value)
                           .topic))
     purpose: (and .?purpose (if (hash-table? .purpose)
                               (hash-get .purpose 'value)
                               .purpose))
     is-channel: .?is_channel
     is-im: .?is_im
     is-mpim: .?is_mpim
     is-private: .?is_private
     is-archived: .?is_archived
     is-member: .?is_member
     creator: .?creator
     created: .?created
     num-members: .?num_members)))

(def (json->message h)
  (let-hash h
    (make-message
     ts: .?ts
     channel: .?channel
     user: .?user
     text: .?text
     thread-ts: .?thread_ts
     reply-count: .?reply_count
     reactions: (if .?reactions
                  (map json->reaction .reactions)
                  [])
     attachments: (or .?attachments [])
     files: (if .?files
              (map json->file-info .files)
              [])
     edited: (and .?edited #t)
     is-bot: (or .?bot_id #f)
     bot-id: .?bot_id)))

(def (json->reaction h)
  (let-hash h
    (make-reaction
     name: .?name
     count: (or .?count 0)
     users: (or .?users []))))

(def (json->file-info h)
  (let-hash h
    (make-file-info
     id: .?id
     name: .?name
     title: .?title
     mimetype: .?mimetype
     url-private: .?url_private
     size: .?size
     user: .?user
     created: .?created)))

(def (json->team h)
  (let-hash h
    (make-team
     id: .?id
     name: .?name
     domain: .?domain
     icon-url: (and .?icon (hash-table? .icon)
                    (hash-get .icon 'image_132)))))

(def (json->reminder h)
  (let-hash h
    (make-reminder
     id: .?id
     creator: .?creator
     user: .?user
     text: .?text
     complete-ts: .?complete_ts)))

(def (json->bookmark h)
  (let-hash h
    (make-bookmark
     id: .?id
     channel-id: .?channel_id
     title: .?title
     type: .?type
     link: .?link
     emoji: .?emoji)))

;;;; Display Helpers

(def (user-display u)
  "Best display name for a user."
  (or (user-display-name u)
      (user-real-name u)
      (user-name u)
      (user-id u)))

(def (channel-display ch)
  "Best display name for a channel."
  (or (channel-name ch)
      (channel-id ch)))

(def (message-summary msg (max-len 80))
  "Truncated message text for display."
  (let ((text (or (message-text msg) "")))
    (if (> (string-length text) max-len)
      (string-append (substring text 0 (- max-len 3)) "...")
      text)))
