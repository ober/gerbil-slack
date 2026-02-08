;; -*- Gerbil -*-
;; Slack reactions API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (reactions-add channel ts name token: (token #f))
  "Add a reaction to a message. name: emoji name without colons."
  (slack-post "reactions.add"
              (list (cons "channel" channel)
                    (cons "timestamp" ts)
                    (cons "name" name))
              token: token)
  #t)

(def (reactions-remove channel ts name token: (token #f))
  "Remove a reaction from a message."
  (slack-post "reactions.remove"
              (list (cons "channel" channel)
                    (cons "timestamp" ts)
                    (cons "name" name))
              token: token)
  #t)

(def (reactions-get channel ts token: (token #f))
  "Get reactions for a message. Returns list of reaction structs."
  (let* ((resp (slack-api "reactions.get"
                          (list (cons "channel" channel)
                                (cons "timestamp" ts)
                                (cons "full" "true"))
                          token: token))
         (msg (hash-get resp 'message))
         (reactions (and msg (hash-table? msg) (hash-get msg 'reactions))))
    (if reactions (map json->reaction reactions) [])))
