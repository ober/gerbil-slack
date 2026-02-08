;; -*- Gerbil -*-
;; Slack usergroups API methods

(import
  :std/sugar
  :ober/slack/http)

(export #t)

(def (usergroups-list include-users: (include-users #f)
                      token: (token #f))
  "List user groups. Returns list of usergroup hashes."
  (let* ((params (if include-users
                   (list (cons "include_users" "true"))
                   []))
         (resp (slack-api "usergroups.list" params token: token)))
    (or (hash-get resp 'usergroups) [])))

(def (usergroups-create name
                        handle: (handle #f)
                        description: (description #f)
                        token: (token #f))
  "Create a user group. Returns response hash."
  (let ((params (append (list (cons "name" name))
                        (if handle (list (cons "handle" handle)) [])
                        (if description (list (cons "description" description)) []))))
    (slack-post "usergroups.create" params token: token)))

(def (usergroups-users-list usergroup-id token: (token #f))
  "List users in a user group. Returns list of user-id strings."
  (let ((resp (slack-api "usergroups.users.list"
                         (list (cons "usergroup" usergroup-id))
                         token: token)))
    (or (hash-get resp 'users) [])))
