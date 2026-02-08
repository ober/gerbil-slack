;; -*- Gerbil -*-
;; Slack auth API methods

(import
  :std/sugar
  :ober/slack/http)

(export #t)

(def (auth-test token: (token #f))
  "Test authentication. Returns response hash with team, user, user_id, team_id, url."
  (slack-api "auth.test" token: token))

(def (auth-revoke token: (token #f))
  "Revoke a token. Returns #t on success."
  (let ((resp (slack-api "auth.revoke" token: token)))
    (let-hash resp
      (or .?revoked #t))))
