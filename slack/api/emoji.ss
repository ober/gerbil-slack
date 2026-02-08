;; -*- Gerbil -*-
;; Slack emoji API methods

(import
  :ober/slack/http)

(export #t)

(def (emoji-list token: (token #f))
  "List custom emoji. Returns hash of name -> url."
  (let ((resp (slack-api "emoji.list" token: token)))
    (or (hash-get resp 'emoji) (make-hash-table))))
