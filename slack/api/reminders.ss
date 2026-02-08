;; -*- Gerbil -*-
;; Slack reminders API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (reminders-add text time
                    user: (user #f)
                    token: (token #f))
  "Add a reminder. time: Unix timestamp or natural language. Returns reminder struct."
  (let* ((params (append (list (cons "text" text)
                               (cons "time" (if (number? time)
                                              (number->string time)
                                              time)))
                         (if user (list (cons "user" user)) [])))
         (resp (slack-post "reminders.add" params token: token))
         (r (hash-get resp 'reminder)))
    (if r (json->reminder r) (error "No reminder in response"))))

(def (reminders-complete reminder-id token: (token #f))
  "Mark a reminder as complete."
  (slack-post "reminders.complete"
              (list (cons "reminder" reminder-id))
              token: token)
  #t)

(def (reminders-delete reminder-id token: (token #f))
  "Delete a reminder."
  (slack-post "reminders.delete"
              (list (cons "reminder" reminder-id))
              token: token)
  #t)

(def (reminders-info reminder-id token: (token #f))
  "Get info about a reminder. Returns reminder struct."
  (let* ((resp (slack-api "reminders.info"
                          (list (cons "reminder" reminder-id))
                          token: token))
         (r (hash-get resp 'reminder)))
    (if r (json->reminder r) (error "No reminder in response"))))

(def (reminders-list token: (token #f))
  "List all reminders. Returns list of reminder structs."
  (let* ((resp (slack-api "reminders.list" token: token))
         (reminders (hash-get resp 'reminders)))
    (if reminders (map json->reminder reminders) [])))
