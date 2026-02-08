;; -*- Gerbil -*-
(import
  :std/db/dbi
  :std/db/sqlite
  :std/sugar
  :std/text/json
  :ober/slack/types
  :ober/slack/events)

(export #t)

(def *cache-db* #f)

(def (current-unix-time)
  (inexact->exact (floor (##current-time-point))))

(def (cache-open! (path #f))
  "Open the cache database. Creates directory and schema if needed.
   Default path: ~/.slack/cache.db"
  (let* ((db-path (or path
                      (path-expand "cache.db"
                                   (path-expand ".slack" (getenv "HOME")))))
         (_ (create-directory* (path-directory db-path)))
         (db (sqlite-open db-path)))
    (set! *cache-db* db)
    (cache-init-schema! db)
    (cache-register-event-handlers!)
    db))

(def (cache-close!)
  (when *cache-db*
    (sql-close *cache-db*)
    (set! *cache-db* #f)))

(def (cache-db)
  (or *cache-db*
      (error "Cache not open")))

(def (cache-init-schema! db)
  (sql-eval db "CREATE TABLE IF NOT EXISTS users (
    id TEXT PRIMARY KEY, name TEXT, real_name TEXT, display_name TEXT,
    email TEXT, avatar_url TEXT, is_bot INTEGER DEFAULT 0,
    json TEXT, updated_at INTEGER)")
  (sql-eval db "CREATE TABLE IF NOT EXISTS channels (
    id TEXT PRIMARY KEY, name TEXT, is_channel INTEGER DEFAULT 0,
    is_im INTEGER DEFAULT 0, is_private INTEGER DEFAULT 0,
    is_archived INTEGER DEFAULT 0, json TEXT, updated_at INTEGER)")
  (sql-eval db "CREATE TABLE IF NOT EXISTS messages (
    ts TEXT, channel TEXT, user_id TEXT, text TEXT, thread_ts TEXT,
    json TEXT, PRIMARY KEY (channel, ts))")
  (sql-eval db "CREATE TABLE IF NOT EXISTS metadata (
    key TEXT PRIMARY KEY, value TEXT)"))

(def (user->json-string u)
  (json-object->string
   (hash ("id" (or (user-id u) ""))
         ("name" (or (user-name u) ""))
         ("real_name" (or (user-real-name u) ""))
         ("display_name" (or (user-display-name u) ""))
         ("email" (or (user-email u) ""))
         ("avatar_url" (or (user-avatar-url u) ""))
         ("is_bot" (if (user-is-bot u) #t #f))
         ("is_admin" (if (user-is-admin u) #t #f))
         ("tz" (or (user-tz u) ""))
         ("status_text" (or (user-status-text u) ""))
         ("status_emoji" (or (user-status-emoji u) ""))
         ("presence" (or (user-presence u) "")))))

(def (json-string->user s)
  (let ((h (parameterize ((read-json-key-as-symbol? #t))
             (call-with-input-string s read-json))))
    (json->user h)))

(def (channel->json-string ch)
  (json-object->string
   (hash ("id" (or (channel-id ch) ""))
         ("name" (or (channel-name ch) ""))
         ("topic" (or (channel-topic ch) ""))
         ("purpose" (or (channel-purpose ch) ""))
         ("is_channel" (if (channel-is-channel ch) #t #f))
         ("is_im" (if (channel-is-im ch) #t #f))
         ("is_mpim" (if (channel-is-mpim ch) #t #f))
         ("is_private" (if (channel-is-private ch) #t #f))
         ("is_archived" (if (channel-is-archived ch) #t #f))
         ("is_member" (if (channel-is-member ch) #t #f))
         ("creator" (or (channel-creator ch) ""))
         ("created" (or (channel-created ch) 0))
         ("num_members" (or (channel-num-members ch) 0)))))

(def (json-string->channel s)
  (let ((h (parameterize ((read-json-key-as-symbol? #t))
             (call-with-input-string s read-json))))
    (json->channel h)))

(def (message->json-string msg)
  (json-object->string
   (hash ("ts" (or (message-ts msg) ""))
         ("channel" (or (message-channel msg) ""))
         ("user" (or (message-user msg) ""))
         ("text" (or (message-text msg) ""))
         ("thread_ts" (or (message-thread-ts msg) #f))
         ("reply_count" (or (message-reply-count msg) 0))
         ("edited" (if (message-edited msg) #t #f))
         ("bot_id" (or (message-bot-id msg) #f)))))

(def (json-string->message s)
  (let ((h (parameterize ((read-json-key-as-symbol? #t))
             (call-with-input-string s read-json))))
    (json->message h)))

(def (cache-user! u)
  (let ((db (cache-db))
        (now (current-unix-time)))
    (sql-eval db
      "INSERT OR REPLACE INTO users (id, name, real_name, display_name, email, avatar_url, is_bot, json, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      (user-id u)
      (user-name u)
      (user-real-name u)
      (user-display-name u)
      (user-email u)
      (user-avatar-url u)
      (if (user-is-bot u) 1 0)
      (user->json-string u)
      now)))

(def (cache-get-user id)
  (let ((rows (sql-eval-query (cache-db)
                "SELECT json FROM users WHERE id = ?" id)))
    (and (pair? rows)
         (json-string->user (car rows)))))

(def (cache-all-users)
  (map json-string->user
       (sql-eval-query (cache-db) "SELECT json FROM users ORDER BY name")))

(def (cache-find-user-by-name name)
  (let ((rows (sql-eval-query (cache-db)
                "SELECT json FROM users WHERE name = ?" name)))
    (and (pair? rows)
         (json-string->user (car rows)))))

(def (cache-channel! ch)
  (let ((db (cache-db))
        (now (current-unix-time)))
    (sql-eval db
      "INSERT OR REPLACE INTO channels (id, name, is_channel, is_im, is_private, is_archived, json, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      (channel-id ch)
      (channel-name ch)
      (if (channel-is-channel ch) 1 0)
      (if (channel-is-im ch) 1 0)
      (if (channel-is-private ch) 1 0)
      (if (channel-is-archived ch) 1 0)
      (channel->json-string ch)
      now)))

(def (cache-get-channel id)
  (let ((rows (sql-eval-query (cache-db)
                "SELECT json FROM channels WHERE id = ?" id)))
    (and (pair? rows)
         (json-string->channel (car rows)))))

(def (cache-all-channels)
  (map json-string->channel
       (sql-eval-query (cache-db) "SELECT json FROM channels ORDER BY name")))

(def (cache-find-channel-by-name name)
  (let ((rows (sql-eval-query (cache-db)
                "SELECT json FROM channels WHERE name = ?" name)))
    (and (pair? rows)
         (json-string->channel (car rows)))))

(def (cache-message! msg)
  (let ((db (cache-db))
        (ch (message-channel msg)))
    (when (and (message-ts msg) ch)
      (sql-eval db
        "INSERT OR REPLACE INTO messages (ts, channel, user_id, text, thread_ts, json)
         VALUES (?, ?, ?, ?, ?, ?)"
        (message-ts msg)
        ch
        (message-user msg)
        (message-text msg)
        (message-thread-ts msg)
        (message->json-string msg)))))

(def (cache-get-messages channel-id (limit 50) (before #f))
  (if before
    (map json-string->message
         (sql-eval-query (cache-db)
           "SELECT json FROM messages WHERE channel = ? AND ts < ? ORDER BY ts DESC LIMIT ?"
           channel-id before limit))
    (map json-string->message
         (sql-eval-query (cache-db)
           "SELECT json FROM messages WHERE channel = ? ORDER BY ts DESC LIMIT ?"
           channel-id limit))))

(def (cache-get-thread channel-id thread-ts)
  (map json-string->message
       (sql-eval-query (cache-db)
         "SELECT json FROM messages WHERE channel = ? AND (thread_ts = ? OR ts = ?) ORDER BY ts ASC"
         channel-id thread-ts thread-ts)))

(def (cache-delete-message! channel-id ts)
  (sql-eval (cache-db)
    "DELETE FROM messages WHERE channel = ? AND ts = ?"
    channel-id ts))

(def (cache-set-meta! key value)
  (sql-eval (cache-db)
    "INSERT OR REPLACE INTO metadata (key, value) VALUES (?, ?)"
    key value))

(def (cache-get-meta key)
  (let ((rows (sql-eval-query (cache-db)
                "SELECT value FROM metadata WHERE key = ?" key)))
    (and (pair? rows) (car rows))))

(def *user-ttl* 3600)
(def *channel-ttl* 300)

(def (cache-users-stale?)
  (let ((last (cache-get-meta "users_updated_at")))
    (or (not last)
        (> (- (current-unix-time) (string->number last))
           *user-ttl*))))

(def (cache-channels-stale?)
  (let ((last (cache-get-meta "channels_updated_at")))
    (or (not last)
        (> (- (current-unix-time) (string->number last))
           *channel-ttl*))))

(def (cache-mark-users-fresh!)
  (cache-set-meta! "users_updated_at" (number->string (current-unix-time))))

(def (cache-mark-channels-fresh!)
  (cache-set-meta! "channels_updated_at" (number->string (current-unix-time))))

(def (cache-register-event-handlers!)
  (on-event 'message
    (lambda (ev)
      (when *cache-db*
        (let ((data (slack-event-data ev)))
          (when (and data (hash-table? data))
            (let ((msg (json->message data)))
              (when (message-ts msg)
                (cache-message! msg))))))))

  (on-event 'message-deleted
    (lambda (ev)
      (when *cache-db*
        (let ((data (slack-event-data ev)))
          (when (and data (hash-table? data))
            (let ((ch (hash-get data 'channel))
                  (ts (hash-get data 'deleted_ts)))
              (when (and ch ts)
                (cache-delete-message! ch ts))))))))

  (on-event 'user-change
    (lambda (ev)
      (when *cache-db*
        (let ((data (slack-event-data ev)))
          (when (and data (hash-table? data))
            (let ((user-data (hash-get data 'user)))
              (when (and user-data (hash-table? user-data))
                (cache-user! (json->user user-data)))))))))

  (on-event 'channel-created
    (lambda (ev)
      (when *cache-db*
        (let ((data (slack-event-data ev)))
          (when (and data (hash-table? data))
            (let ((ch-data (hash-get data 'channel)))
              (when (and ch-data (hash-table? ch-data))
                (cache-channel! (json->channel ch-data))))))))))

(def (cache-stats)
  (let ((db (cache-db)))
    (hash
     ("users" (car (sql-eval-query db "SELECT COUNT(*) FROM users")))
     ("channels" (car (sql-eval-query db "SELECT COUNT(*) FROM channels")))
     ("messages" (car (sql-eval-query db "SELECT COUNT(*) FROM messages"))))))
