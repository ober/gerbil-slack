;;; -*- Gerbil -*-
;;; © ober
;;; Slack client binary

(import
  :gerbil/gambit
  :gerbil/gambit/bits
  :gerbil/gambit/misc
  :gerbil/gambit/os
  :gerbil/gambit/ports
  :gerbil/gambit/threads
  :scheme/base
  :std/actor/message
  :std/actor/proto
  :std/coroutine
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/error
  :std/format
  :std/generic
  :std/iter
  :std/logger
  :std/misc/completion
  :std/misc/list
  :std/misc/threads
  :std/net/request
  :std/net/websocket
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :ober/oberlib
  :ober/slack/client)

(declare (not optimize-dead-definitions))

(export main)

(def interactives
  (hash
   ("channel-history" (hash (description: "Channel list.") (usage: "channel-history <channel id>") (count: 1)))
   ("channels" (hash (description: "Channel list.") (usage: "channels") (count: 0)))
   ("chats" (hash (description: "Chat list.") (usage: "chats") (count: 0)))
   ("config" (hash (description: "Set your encrypted password.") (usage: "config") (count: 0)))
   ("delete" (hash (description: "Delete a message.") (usage: "delete <channel> <timestamp>") (count: 2)))
   ("emojis" (hash (description: "Channel list.") (usage: "channels") (count: 0)))
   ("ghistory" (hash (description: "Group history.") (usage: "ghistory <group>") (count: 1)))
   ("groups" (hash (description: "Group list.") (usage: "groups") (count: 0)))
   ("gul" (hash (description: "Channel list.") (usage: "channels") (count: 0)))
   ("gw" (hash (description: "Send message to user.") (usage: "gw <group> <channel> <message>") (count: 3)))
   ("im-history" (hash (description: "IM History.") (usage: "im-history <name of user>") (count: 1)))
   ("im-open" (hash (description: "Open chat with user") (usage: "chat-open user") (count: 1)))
   ("list-records" (hash (description: "list records.") (usage: "list-records") (count: 0)))
   ("msg" (hash (description: "Send message to user.") (usage: "msg <username> <message>") (count: 2)))
   ("post" (hash (description: "Post IM message to channel.") (usage: "post <channel> <message> <from>") (count: 3)))
   ("rtm-start" (hash (description: "Start realtime chat.") (usage: "rtm-start") (count: 0)))
   ("rtm-start-json" (hash (description: "Start realtime chat.") (usage: "rtm-start") (count: 0)))
   ("search" (hash (description: "Search messages for pattern.") (usage: "searchm <pattern>") (count: 1)))
   ("set-topic" (hash (description: "Set topic on channel") (usage: "set-topic <channel> <topic>") (count: 2)))
   ("user" (hash (description: "Describe user") (usage: "user <username>") (count: 1)))
   ("users" (hash (description: "List All Slack Users") (usage: "users") (count: 0)))
   ("whisper" (hash (description: "Send private message to user in channel") (usage: "whisper <username> <channel> <message>") (count: 3)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
         (verb (car args))
         (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
           (count (hash-get info count:)))
      (unless count
        (set! count 0))
      (unless (= (length args2) count)
        (usage-verb verb))
      (apply (eval (string->symbol (string-append "slack#" verb))) args2))))

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
