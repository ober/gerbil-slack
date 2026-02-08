;; -*- Gerbil -*-
;; Slack DND (Do Not Disturb) API methods

(import
  :std/sugar
  :ober/slack/http)

(export #t)

(def (dnd-set-snooze num-minutes token: (token #f))
  "Enable DND for specified number of minutes."
  (slack-post "dnd.setSnooze"
              (list (cons "num_minutes" (number->string num-minutes)))
              token: token)
  #t)

(def (dnd-end-snooze token: (token #f))
  "End the current DND snooze."
  (slack-post "dnd.endSnooze" token: token)
  #t)

(def (dnd-info user: (user #f) token: (token #f))
  "Get DND info. Returns response hash."
  (let ((params (if user (list (cons "user" user)) [])))
    (slack-api "dnd.info" params token: token)))
