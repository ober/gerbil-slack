;; -*- Gerbil -*-
package: slack
namespace: slack

(export main)

(declare (not optimize-dead-definitions))
(def version "0.02")

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
  :std/text/yaml)

(def program-name "slack")
(def config-file "~/.slack.yaml")

(def user-list (hash))
(def channel-list (hash))

(def DEBUG (getenv "DEBUG" #f))

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

(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (float->int num)
  (inexact->exact
   (round num)))

(def (epoch->date epoch)
  (cond
   ((string? epoch)
    (time-utc->date (make-time time-utc 0 (string->number epoch))))
   ((flonum? epoch)
    (time-utc->date (make-time time-utc 0 (float->int epoch))))
   ((fixnum? epoch)
    (time-utc->date (make-time time-utc 0 epoch)))))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (print-date date)
  (date->string date "~c"))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (let* ((reply (http-post uri
                           headers: headers
                           data: data))
         (status (request-status reply))
         (text (request-text reply)))

    (if (success? status)
      text
      (format "Failure on post. Status:~a Text:~a~%" status text))))

(def (do-get uri)
  (let* ((reply (http-get uri))
         (status (request-status reply))
         (text (request-text reply)))
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (from-json json)
  (with-input-from-string json read-json))

(def (hash->str h)
  (let ((results []))
    (if (table? h)
      (begin
        (hash-for-each
         (lambda (k v)
           (set! results (cons  [ (format " ~a->" k) (format "~a   " v)]   results)))
         h)
        (append-strings results))
      ;;        (pregexp-replace "\n" (append-strings results) "\t"))
      "N/A")))

(def (get-chat-list)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/im.list?token=~a" .token))
           (results (do-get uri))
           (myjson (from-json results))
           (status (hash-get myjson 'status))
           (ims (hash-get myjson 'ims)))
      ims)))


(def (chats)
  (let ((chats (get-chat-list))
        (outs [[ "user" "id" "user-id" "priority" "is_user_deleted" "is_archived" "is_im" "created" "is_org_shared" ]]))
    (for (chat chats)
         (let-hash chat
           (set! outs (cons [
                             (id-for-user .?user)
                             .?id
                             .?user
                             .?priority
                             .?is_user_deleted
                             .?is_archived
                             .?is_im
                             .?created
                             .?is_org_shared
                             ] outs))))
    (style-output outs)))

(def (ghistory group)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/groups.history?token=~a&channel=~a&count=~a" .token group 2000))
           (results (do-get uri))
           (myjson (from-json results)))
      (displayln results))))

(def (user user)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.info?token=~a&user=~a" .token user))
           (results (do-get uri))
           (myjson (from-json results)))
      (displayln results))))

(def (groups)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/groups.list?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (groups (hash-get myjson 'groups)))
      (displayln "|name|creator|is_group|purpose|members|created|name2|id |is_archived|is_mpim|topic|priority|")
      (displayln "|--|-------|-----|-------------|----|---------------|--------|")
      (for (g groups)
           (let-hash g
             (displayln "|" .name_normalized
                        "|" (hash-ref .topic 'value)
                        "|" .creator
                        "|" .is_group
                        "|" (hash-ref .purpose 'value)
                        "|" .members
                        "|" .created
                        "|" .name
                        "|" .id
                        "|" .is_archived
                        "|" .is_mpim
                        "|" .priority "|"))))))

(def (post channel message from)
  (let-hash (load-config)
    (let* ((uri "https://slack.com/api/chat.postMessage")
	   (data (json-object->string
		  (hash
		   ("as_user" #t)
		   ("username" from)
		   ("text" message)
		   ("channel" channel))))
	   (results (do-post uri (default-headers) data)))
      (displayln results))))

(def (im-open name)
  (let* ((uri "https://slack.com/api/im.open")
	 (id (id-for-user name))
	 (data (json-object->string
		(hash
		 ("user" id)
		 ("return_im" #t))))
	 (results (do-post uri (default-headers) data))
	 (myjson (from-json results)))
    (let-hash myjson
      (when .?channel
        (let-hash .channel
          .id)))))

(def (msg user message)
  (let-hash (load-config)
    (let ((channel (im-open user)))
      (if channel
	(displayln (post channel message .username))
	(displayln "Invalid user: " user)))))


(def (emojis)
  (let-hash (load-config)
    (let*
	((uri (format "https://slack.com/api/emoji.list?token=~a" .token))
	 (results (do-get uri))
	 (myjson (from-json results)))
      (displayln results))))

(def (delete channel timestamp)
  "Remove a message from a chat channel"
  (let-hash (load-config)
    (let* ((uri "https://slack.com/api/chat.delete")
	   (data (json-object->string
		  (hash
		   ("as_user" #t)
		   ("ts" timestamp)
		   ("channel" channel))))
	   (results (do-post uri (default-headers) data)))
      (displayln results))))

(def (search query)
  (let-hash (load-config)
    (let*
	((uri (format "https://slack.com/api/search.messages?token=~a&count=~a&query=~a" .token 100 query))
	 (results (do-get uri))
         (outs [[ "id" "channel" "Message" "channel-id" "ts" "type" "user" "permalink" ]])
	 (myjson (from-json results)))
      (let-hash myjson
	(if .?messages
	  (let-hash .?messages
	    (if .?matches
	      (let-hash .?matches
                (for (m ..matches)
                     (let-hash m
                       (set! outs (cons [ .username
                                          (hash-get .channel 'name)
                                          .text
                                          (hash-get .channel 'id)
                                          .ts
                                          .type
                                          .user
                                          .permalink ] outs)))))))))
      (style-output outs))))



(def (gul)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
	   (results (do-get uri)))
           (displayln results))))

(def (get-user-list)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (members (hash-ref myjson 'members)))
      members)))

(def (users)
  (let* ((members (get-user-list))
         (outs [[ "name" "real_name" "is_primary_owner" "is_ultra_restricted" "is_restricted" "team_id" "updated" "is_app_user" "profile" "id" "tz_label" ]]))
    (for (user members)
         (let-hash user
           (set! outs (cons [
                             .?name
                             .?real_name
                             .?is_primary_owner
                             .?is_ultra_restricted
                             .?is_restricted
                             .?team_id
                             .?updated
                             .?is_app_user
                             (hash->list .profile)
                             .?id
                             .?tz_label ] outs))))
    (style-output outs)))

(def (id-for-user user)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
           (results (do-get uri))
           (myjson (from-json results))
           (members (hash-ref myjson 'members))
           (id #f))
      (for (u members)
           (let-hash u
             (when (string=? .name user)
               (set! id .id))))
      id)))

(def (id-for-channel channel)
  (let ((id #f))
    (let-hash (get-channel-list)
      (for (c .channels)
           (let-hash c
             (when (string=? .name channel)
               (set! id .id)))))
    id))

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

(def (get-channel-list)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/channels.list?token=~a" .token))
           (headers '(("Content-type" . "application/json")))
           (results (do-get uri))
           (channels (from-json results)))
      channels)))

(def (channels)
  (let-hash (get-channel-list)
    (print-channels .channels)))

(def (users-hash)
  (let ((users (hash))
        (members (get-user-list)))
    (for (member members)
         (let-hash member
           (hash-put! users (string->symbol .id) .name)))
    users))

(def (user-from-id id users-hash)
  (if id
    (hash-get users-hash (string->symbol id))
    id))

(def (im-history user)
  (let-hash (load-config)
    (let* ((users-hash (users-hash))
           (id (id-for-channel user))
           (uri (format "https://slack.com/api/im.history?token=~a&channel=~a&count=1000" .token id))
           (headers '(("Content-type" . "application/json")))
           (outs [[ "User" "Channel" "Message" "channel-id" "ts" "team" ]])
           (results (do-get uri))
           (myjson (from-json results)))
      (let-hash myjson
        (for (message .messages)
             (dp (table->list message))
             (let-hash message
               (set! outs (cons [ (user-from-id .?user users-hash)
                                  user
                                  .?text
                                  id
                                  .?ts
                                  .?team ] outs)))))
      (style-output outs))))

(def (channel-history channel)
  (let-hash (load-config)
    (let* ((users-hash (users-hash))
           (id (id-for-channel channel))
           (uri (format "https://slack.com/api/channels.history?token=~a&channel=~a&count=1000" .token id))
           (headers '(("Content-type" . "application/json")))
           (outs [[ "User" "Channel" "Message" "channel-id" "ts" "team" ]])
           (results (do-get uri))
           (myjson (from-json results)))
      (let-hash myjson
        (for (message .messages)
             (dp (table->list message))
             (let-hash message
               (set! outs (cons [ (user-from-id .?user users-hash)
                                  channel
                                  .?text
                                  id
                                  .?ts
                                  .?team ] outs)))))
      (style-output outs))))

(def (print-channels channels)
  (when (list? channels)
    (for (channel channels)
         (print-channel channel))))

(def (print-channel channel)
  (if (table? channel)
    (let-hash channel
      (displayln "is_channel: " .?is_channel
                 " created: " .?created
                 " is_member: " .?is_member
                 " is_mpim: " .?is_mpim
                 " previous_names: " .?previous_names
                 " id: " .?id
                 " unlinked: " .?unlinked
                 " priority: " .?priority
                 " has_pins: " .?has_pins
                 " name: " .?name
                 " is_archived: " .?is_archived
                 " is_general: " .?is_general
                 " creator: " .?creator
                 " name_normalized: " .?name_normalized
                 " is_org_shared: " .?is_org_shared
                 " is_private: " .?is_private))))

(def (set-topic channel topic)
  (let* ((uri "https://slack.com/api/channels.setTopic")
	 (data (json-object->string
		(hash
		 ("topic" topic)
		 ("channel" channel))))
	 (results (do-post uri (default-headers) data)))
    (displayln results)))

(def (print-curl type uri headers data)
  (let ((heads "Content-type: application/json")
	(do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
	(if (string=? "" data)
	  (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
	  (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
	(displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
	(displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" headers data uri)))
       (else
	(displayln "unknown format " type))))))

(def (load-config)
  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (table? (car config-data)))
      (displayln (format "Could not parse your config ~a" config-file))
      (exit 2))

    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car config-data))

    (let-hash config
      (when (and .?key .?iv .?password)
	(hash-put! config 'token (get-password-from-config .key .iv .password)))
      config)))

(def (default-headers)
  (let-hash (load-config)
    [
     ["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]
     ]))

(def (whisper user channel msg)
  (let-hash (load-config)
    (let* ((uri "https://slack.com/api/chat.postEphemeral")
	   (data (json-object->string
		  (hash
		   ("as_user" #t)
		   ("attachments" #f)
		   ("channel" channel)
		   ("id" (im-open user))
		   ("link_names" #t)
		   ("parse" "none")
		   ("user" (id-for-user user))
		   ("text" msg))))
	   (results (do-post uri (default-headers) data)))
      (displayln results))))

(def (config)
  (let-hash (load-config)
    (displayln "Please enter your slack token")
    (let* ((password (read-line (current-input-port)))
	   (cipher (make-aes-256-ctr-cipher))
	   (iv (random-bytes (cipher-iv-length cipher)))
	   (key (random-bytes (cipher-key-length cipher)))
	   (encrypted-password (encrypt cipher key iv password))
	   (enc-pass-store (u8vector->base64-string encrypted-password))
	   (iv-store (u8vector->base64-string iv))
	   (key-store (u8vector->base64-string key)))
      (displayln "Add the following lines to your " config-file)
      (displayln "-----------------------------------------")
      (displayln "password: " enc-pass-store)
      (displayln "iv: " iv-store)
      (displayln "key: " key-store)
      (displayln "-----------------------------------------"))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (list-records)
  (displayln "records here"))

(def (def-num num)
  (if (string? num)
    (string->number num)
    num))

(def (rtm-start-json)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/rtm.start?token=~a" .token))
	   (results (do-get uri)))
      (displayln results))))

(def (rtm-start)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/rtm.start?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (status (hash-get myjson 'status)))
      (let-hash myjson
	(displayln "self: " (hash->list .self))
	(displayln "OK: " .ok)
	(displayln "dnd: " (if (and .?dnd (table? .?dnd)) (hash->list .dnd)))
	(displayln "url: " .?url)
	(displayln "Channels: ------------------------------------------------------------")
	(print-channels .?channels)
	(displayln "Users: ------------------------------------------------------------")
	;;(print-users .?users)
        (for (user .?users)
             (displayln (hash->list user)))

	(displayln "Bots: ------------------------------------------------------------")
        (for (bot .?bots)
             (displayln (hash->list bot)))

	(displayln "team: " (hash->list .?team))
	(displayln "groups: " .?groups)
	(displayln "self: " .?self)
	(displayln "ims: " .?ims)
	(displayln "thread_only_channels: " .?thread_only_channels)
	(displayln "non_threadable_channels: " .?non_threadable_channels)
	(displayln "subteams: " .?subteams)
	(displayln "cache_version: " .?cache_version)
	(displayln "cache_ts_version: " .?cache_ts_version)
	(displayln "latest_event_ts: " .?latest_event_ts)
	(displayln "can_manage_shared_channels: " .?can_manage_shared_channels)
	(displayln "cache_ts: " .?cache_ts)
	(displayln "read_only_channels: " .?read_only_channels)))))

(def (slack-send ws msg)
  (let (outp (open-output-u8vector))
    (write-json msg outp)
    (websocket-send ws (get-output-u8vector outp) 'text)))

(def (slack-recv ws)
  (let ((values bytes type) (websocket-recv ws))
    (if (eq? type 'text)
      (read-json (open-input-u8vector [char-encoding: 'UTF-8 init: bytes]))
      (begin
        (warning "wamp-recv: server sent binary data (~s)" (u8vector-length bytes))
        (raise-io-error 'wamp-recv "server sent binary data" bytes)))))

(def (gw group channel message)
  "Take a group and whisper the message to each member in the channel"
  (let-hash (load-config)
    (when .?groups
      (when (table? .groups)
	(if (hash-ref .groups group)
	  (for (user (hash-ref .groups group))
	       (whisper user channel message))
	  (displayln "Error: group" group " not found in " .groups))))))

(def (style-output infos)
  (let-hash (load-config)
    (when (list? infos)
      (let* ((sizes (hash))
	     (data (reverse infos))
	     (header (car data))
	     (rows (cdr data)))
	(for (head header)
	     (unless (string? head) (displayln "head is not string: " head) (exit 2))
	     (hash-put! sizes head (string-length head)))
	(for (row rows)
	     (let (count 0)
	       (for (column row)
		    (let* ((col-name (nth count header))
			   (current-size (hash-ref sizes col-name))
			   (this-size (if (string? column) (string-length column) (string-length (format "~a" column)))))
		      (when (> this-size current-size)
			(hash-put! sizes col-name this-size))
                      ;;		      (displayln "colname: " col-name " col: " count " current-size: " current-size " this-size: " this-size " column: " column)
		      (set! count (1+ count))))))

	(for (head header)
	     (display (format "| ~a" (format-string-size head (hash-get sizes head)))))

	;; print header
	(displayln "|")
	(let ((count 0))
	  (for (head header)
	       (let ((sep (if (= count 0) "|" "+")))
		 (display (format "~a~a" sep (make-string (+ 2 (hash-get sizes (nth count header))) #\-))))
	       (set! count (1+ count))))
	(displayln "|")

	(for (row rows)
	     (let (count 0)
	       (for (col row)
		    (display (format "|~a " (format-string-size col (hash-ref sizes (nth count header)))))
		    (set! count (1+ count))))
	     (displayln "|"))
	))))

(def (nth n l)
  "Implement nth for gerbil. fetch n argument from list"
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(def (format-string-size string size)
  (unless (string? string)
    (set! string (format "~a" string)))
  (let* ((string (string-trim-both string))
	 (our-size (string-length string))
	 (delta (if (> size our-size)
		  (- size our-size)
		  0)))
    (format " ~a~a" string (make-string delta #\space))))

(def (sis item)
  (if item
    item
    "N/A"))
