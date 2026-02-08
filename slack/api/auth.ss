;; -*- Gerbil -*-
;; Slack auth API methods

(import
  :ober/slack/http)

(export #t)

(def (auth-test token: (token #f))
  "Test authentication. Returns response with team, user, user_id, team_id, url."
  (slack-api "auth.test" token: token))

(def (auth-revoke token: (token #f))
  "Revoke a token."
  (let ((resp (slack-api "auth.revoke" token: token)))
    (hash-get resp 'revoked)))
