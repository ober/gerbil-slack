;; -*- Gerbil -*-
;; Slack pins API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (pins-add channel ts token: (token #f))
  "Pin a message to a channel."
  (slack-post "pins.add"
              (list (cons "channel" channel)
                    (cons "timestamp" ts))
              token: token)
  #t)

(def (pins-remove channel ts token: (token #f))
  "Unpin a message from a channel."
  (slack-post "pins.remove"
              (list (cons "channel" channel)
                    (cons "timestamp" ts))
              token: token)
  #t)

(def (pins-list channel token: (token #f))
  "List pinned items in a channel. Returns list of message structs."
  (let* ((resp (slack-api "pins.list"
                          (list (cons "channel" channel))
                          token: token))
         (items (hash-get resp 'items)))
    (if items
      (map (lambda (item)
             (let ((msg (hash-get item 'message)))
               (if msg (json->message msg) item)))
           items)
      [])))
