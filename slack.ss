package: slack
namespace: slack

(export main)

(declare (not optimize-dead-definitions))
(def version "0.01")

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
  :std/db/leveldb
  :std/error
  :std/format
  :std/generic
  :std/iter
  :std/logger
  :std/misc/completion
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
  )

(def program-name "slack")
(def config-file "~/.slack.yaml")

(def user-list (hash))
(def channel-list (hash))

(def DEBUG (getenv "DEBUG" #f))

(def interactives
  (hash
   ("channels" (hash (description: "Channel list.") (usage: "channels") (count: 0)))
   ("chats" (hash (description: "Chat list.") (usage: "chats") (count: 0)))
   ("config" (hash (description: "Set your encrypted password.") (usage: "config") (count: 0)))
   ("ghistory" (hash (description: "Group history.") (usage: "ghistory <group>") (count: 1)))
   ("groups" (hash (description: "Group list.") (usage: "groups") (count: 0)))
   ("gw" (hash (description: "Send message to user.") (usage: "gw <group> <channel> <message>") (count: 3)))
   ("id-for-user" (hash (description: "Open chat with user") (usage: "id-for-user user") (count: 1)))
   ("im-open" (hash (description: "Open chat with user") (usage: "chat-open user") (count: 1)))
   ("list-records" (hash (description: "list records.") (usage: "list-records") (count: 0)))
   ("msg" (hash (description: "Send message to user.") (usage: "msg <username> <message>") (count: 2)))
   ("delete" (hash (description: "Delete a message.") (usage: "delete <channel> <timestamp>") (count: 2)))
   ("post" (hash (description: "Post IM message to channel.") (usage: "post <channel> <message> <from>") (count: 3)))
   ("rtm-start" (hash (description: "Start realtime chat.") (usage: "rtm-start") (count: 0)))
   ("rtm-start-json" (hash (description: "Start realtime chat.") (usage: "rtm-start") (count: 0)))
   ("search" (hash (description: "Search messages for pattern.") (usage: "searchm <pattern>") (count: 1)))
   ("set-topic" (hash (description: "Set topic on channel") (usage: "set-topic <channel> <topic>") (count: 2)))
   ("user" (hash (description: "Open chat with user") (usage: "user user") (count: 1)))
   ("users" (hash (description: "user list.") (usage: "users") (count: 0)))
   ("whisper" (hash (description: "Send message to user.") (usage: "whisper <username> <channel> <message>") (count: 3)))
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

(def (chats)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/im.list?token=~a" .token))
           (results (do-get uri))
           (myjson (from-json results))
           (status (hash-get myjson 'status))
           (ims (hash-get myjson 'ims)))
      (displayln "|id|created|is_im|is_org_shared|user|is_user_deleted|priority|")
      (displayln "|--|-------|-----|-------------|----|---------------|--------|")
      (for-each
        (lambda (im)
          (let-hash im
            (displayln "|" .id
                       "|" .created
                       "|" .is_im
                       "|" .is_org_shared
                       "|" .user
                       "|" .priority "|")))
        ims))))

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
      (for-each
	(lambda (g)
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
		       "|" .priority "|")))
	groups))))

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
      (let-hash .channel
	.id))))

(def (msg user message)
  (let-hash (load-config)
    (let ((channel (im-open user)))
      (if channel
	(displayln (post channel message .username))
	(displayln "Invalid user: " user)))))


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
	 (myjson (from-json results)))
      (let-hash myjson
	(if .?messages
	  (let-hash .?messages
	    (if .?matches
	      (let-hash .?matches
		;; 	 (matches (hash-get (hash-get myjson 'messages) 'matches)))

		(displayln "|id|team|channel|type|user|username|ts|text|permalink|")
		(displayln "|--|-------|-----|-------------|----|---------------|--------|")
		(for-each
      		  (lambda (m)
      		    (let-hash m
       		      (displayln "|" .username
      	  			 "|" (format "~a:~a" (hash-get .channel 'name) (hash-get .channel 'id))
      	  			 "|" .text
      	  			 "|" .type
      	  			 "|" .user
      	  			 "|" .ts
      	  			 "|" .permalink "|")))
      		  ..matches)))))))))

(def (users)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (members (hash-ref myjson 'members)))
      (print-users members))))

(def (print-users users)
  (when (list? users)
    (for-each
      (lambda (user)
	(print-user user))
      users)))

(def (print-user user)
  (when (table? user)
    (let-hash user
      (displayln "name: " .?name
		 " real_name: " .?real_name
		 " is_primary_user: " .?is_primary_owner
		 " is_ultra_restricted: " .?is_ultra_restricted
		 " is_restricted: " .?is_restricted
		 " team_id: " .?team_id
		 " updated: " .?updated
		 " is_app_user: " .?is_app_user
		 " profile: " (hash->list .profile)
		 " id: " .?id
		 " tz_label" .?tz_label))))

(def (id-for-user user)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (members (hash-ref myjson 'members))
	   (id #f))
      (for-each
	(lambda (u)
    	  (let-hash u
	    (when (string=? .name user)
	      (set! id .id))))
	members)
      id)))


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
  (for-each
    (lambda (k)
      (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
    (sort! (hash-keys interactives) string<?))
  (exit 2))

(def (channels)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/channels.list?token=~a" .token))
	   (headers '(("Content-type" . "application/json")))
	   (results (do-get uri))
	   (channels (from-json results)))
      (print-channels (hash-ref channels 'channels)))))

(def (print-channels channels)
  (when (list? channels)
    (for-each
      (lambda (channel)
	(print-channel channel))
      channels)))

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
  (let ((config (hash)))
    (hash-for-each
     (lambda (k v)
       (hash-put! config (string->symbol k) v))
     (car (yaml-load config-file)))
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
	(print-users .?users)
	(for-each
	  (lambda (user)
	    (displayln (hash->list user)))
	  .?users)

	(displayln "Bots: ------------------------------------------------------------")
	(for-each
	  (lambda (bot)
	    (displayln (hash->list bot)))
	  .?bots)

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
