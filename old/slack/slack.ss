;;; -*- Gerbil -*-
;;; © ober
;;; Slack client binary

(import
  :gerbil/gambit
  :std/format
  :std/iter
  :std/sort
  :std/sugar
  :ober/slack/client)

(export main)

(def interactives
  (hash
   ("active" (hash (description: "Set your status to Active.") (usage: "active") (count: 0) (handler: active)))
   ("auth-test" (hash (description: "Check your token permissions.") (usage: "auth-test") (count: 0) (handler: auth-test)))
   ("away" (hash (description: "Set your status to Away.") (usage: "away") (count: 0) (handler: away)))
   ("channel-history" (hash (description: "Channel history.") (usage: "channel-history <channel id>") (count: 1) (handler: channel-history)))
   ("channels" (hash (description: "Channel list.") (usage: "channels") (count: 0) (handler: channels)))
   ("chats" (hash (description: "Chat list.") (usage: "chats") (count: 0) (handler: chats)))
   ("config" (hash (description: "Set your encrypted password.") (usage: "config") (count: 0) (handler: config)))
   ("delete" (hash (description: "Delete a message.") (usage: "delete <channel> <timestamp>") (count: 2) (handler: delete)))
   ("emojis" (hash (description: "List custom emojis.") (usage: "emojis") (count: 0) (handler: emojis)))
   ("ghistory" (hash (description: "Group history.") (usage: "ghistory <group>") (count: 1) (handler: ghistory)))
   ("groups" (hash (description: "Group list.") (usage: "groups") (count: 0) (handler: groups)))
   ("gul" (hash (description: "List all groups.") (usage: "gul") (count: 0) (handler: gul)))
   ("gw" (hash (description: "Send message to group members.") (usage: "gw <group> <channel> <message>") (count: 3) (handler: gw)))
   ("im-history" (hash (description: "IM History.") (usage: "im-history <name of user>") (count: 1) (handler: im-history)))
   ("im-open" (hash (description: "Open chat with user.") (usage: "im-open <user>") (count: 1) (handler: im-open)))
   ("msg" (hash (description: "Send message to user.") (usage: "msg <username> <message>") (count: 2) (handler: msg)))
   ("post" (hash (description: "Post message to channel.") (usage: "post <channel> <message> <from>") (count: 3) (handler: post)))
   ("presence" (hash (description: "Get status of user Away/Active.") (usage: "presence <username>") (count: 1) (handler: presence)))
   ("rtm-start" (hash (description: "Start realtime messaging.") (usage: "rtm-start") (count: 0) (handler: rtm-start)))
   ("rtm-start-json" (hash (description: "Start realtime messaging (JSON).") (usage: "rtm-start-json") (count: 0) (handler: rtm-start-json)))
   ("search" (hash (description: "Search messages for pattern.") (usage: "search <pattern>") (count: 1) (handler: search)))
   ("set-topic" (hash (description: "Set topic on channel.") (usage: "set-topic <channel> <topic>") (count: 2) (handler: set-topic)))
   ("user" (hash (description: "Describe user.") (usage: "user <username>") (count: 1) (handler: user)))
   ("users" (hash (description: "List all Slack users.") (usage: "users") (count: 0) (handler: users)))
   ("whisper" (hash (description: "Send private message to user in channel.") (usage: "whisper <username> <channel> <message>") (count: 3) (handler: whisper)))
   ))

(def (main . args)
  (when (null? args)
    (usage))
  (let* ((verb (car args))
         (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
           (count (or (hash-get info count:) 0)))
      (unless (= (length args2) count)
        (usage-verb verb))
      (apply (hash-ref info handler:) args2))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln (format "Slack: version ~a" version))
  (displayln "Verbs:")
  (for (k (sort! (hash-keys interactives) string<?))
    (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
  (exit 2))
