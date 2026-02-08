;; -*- Gerbil -*-
;; Slack chat API methods

(import
  :std/sugar
  :ober/slack/http)

(export #t)

(def (chat-post-message channel text
                        thread-ts: (thread-ts #f)
                        unfurl-links: (unfurl-links #f)
                        unfurl-media: (unfurl-media #f)
                        token: (token #f))
  "Post a message to a channel. Returns message timestamp string."
  (let* ((params (append (list (cons "channel" channel)
                               (cons "text" text))
                         (if thread-ts (list (cons "thread_ts" thread-ts)) [])
                         (if (eq? unfurl-links #f) [] (list (cons "unfurl_links" (if unfurl-links "true" "false"))))
                         (if (eq? unfurl-media #f) [] (list (cons "unfurl_media" (if unfurl-media "true" "false"))))))
         (resp (slack-post "chat.postMessage" params token: token)))
    (hash-get resp 'ts)))

(def (chat-update channel ts text token: (token #f))
  "Update a message. Returns #t on success."
  (slack-post "chat.update"
              (list (cons "channel" channel)
                    (cons "ts" ts)
                    (cons "text" text))
              token: token)
  #t)

(def (chat-delete channel ts token: (token #f))
  "Delete a message. Returns #t on success."
  (slack-post "chat.delete"
              (list (cons "channel" channel)
                    (cons "ts" ts))
              token: token)
  #t)

(def (chat-get-permalink channel ts token: (token #f))
  "Get a permalink for a message. Returns URL string."
  (let ((resp (slack-api "chat.getPermalink"
                         (list (cons "channel" channel)
                               (cons "message_ts" ts))
                         token: token)))
    (hash-get resp 'permalink)))

(def (chat-post-ephemeral channel user text token: (token #f))
  "Post an ephemeral message visible only to one user."
  (slack-post "chat.postEphemeral"
              (list (cons "channel" channel)
                    (cons "user" user)
                    (cons "text" text))
              token: token)
  #t)

(def (chat-schedule-message channel text post-at token: (token #f))
  "Schedule a message. post-at: Unix timestamp. Returns scheduled_message_id."
  (let ((resp (slack-post "chat.scheduleMessage"
                          (list (cons "channel" channel)
                                (cons "text" text)
                                (cons "post_at" (if (number? post-at)
                                                  (number->string post-at)
                                                  post-at)))
                          token: token)))
    (hash-get resp 'scheduled_message_id)))

(def (chat-me-message channel text token: (token #f))
  "Post a /me message."
  (slack-post "chat.meMessage"
              (list (cons "channel" channel)
                    (cons "text" text))
              token: token)
  #t)
