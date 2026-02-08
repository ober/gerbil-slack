;; -*- Gerbil -*-
;; Slack team API methods

(import
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (team-info token: (token #f))
  "Get team info. Returns team struct."
  (let* ((resp (slack-api "team.info" token: token))
         (t (hash-get resp 'team)))
    (if t (json->team t) (error "No team in response"))))
