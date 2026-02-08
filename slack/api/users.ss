;; -*- Gerbil -*-
;; Slack users API methods

(import
  :std/sugar
  :std/text/json
  :ober/slack/types
  :ober/slack/http)

(export #t)

;; Caches
(def *user-list-cache* #f)
(def *user-hash-cache* #f)

(def (users-list limit: (limit 200) token: (token #f))
  "List all users in the workspace. Returns list of user structs (paginated)."
  (let ((members (slack-api-paginated "users.list" []
                                       key: 'members
                                       limit: limit
                                       token: token)))
    (map json->user members)))

(def (users-info user-id token: (token #f))
  "Get info about a single user. Returns user struct."
  (let* ((resp (slack-api "users.info"
                          (list (cons "user" user-id))
                          token: token))
         (u (hash-get resp 'user)))
    (if u (json->user u) (error "No user in response"))))

(def (users-get-presence user-id token: (token #f))
  "Get user's presence. Returns symbol: 'active or 'away."
  (let ((resp (slack-api "users.getPresence"
                         (list (cons "user" user-id))
                         token: token)))
    (let-hash resp
      (string->symbol (or .?presence "away")))))

(def (users-set-presence presence token: (token #f))
  "Set your presence. presence should be 'auto or 'away."
  (slack-post "users.setPresence"
              (list (cons "presence" (symbol->string presence)))
              token: token)
  #t)

(def (users-set-status text emoji
                       expiration: (expiration 0)
                       token: (token #f))
  "Set your status text and emoji."
  (slack-post "users.profile.set"
              (list (cons "profile"
                          (json-object->string
                           (hash ("status_text" text)
                                 ("status_emoji" emoji)
                                 ("status_expiration" expiration)))))
              token: token)
  #t)

;;; Cached lookup helpers

(def (get-user-list token: (token #f))
  "Get user list, cached for process lifetime."
  (or *user-list-cache*
      (let ((result (users-list token: token)))
        (set! *user-list-cache* result)
        result)))

(def (users-hash token: (token #f))
  "Build hash: user-id -> user-name. Cached."
  (or *user-hash-cache*
      (let ((h (make-hash-table)))
        (for-each (lambda (u)
                    (hash-put! h (user-id u) (user-name u)))
                  (get-user-list token: token))
        (set! *user-hash-cache* h)
        h)))

(def (user-id-for-name name token: (token #f))
  "Look up user ID by username."
  (let ((found #f))
    (for-each (lambda (u)
                (when (and (user-name u) (string=? (user-name u) name))
                  (set! found (user-id u))))
              (get-user-list token: token))
    found))

(def (user-name-for-id id token: (token #f))
  "Look up username by user ID."
  (hash-get (users-hash token: token) id))

(def (reset-user-cache!)
  "Clear user caches."
  (set! *user-list-cache* #f)
  (set! *user-hash-cache* #f))
