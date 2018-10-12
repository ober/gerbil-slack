package: slack
namespace: slack
(export main)

(declare (not optimize-dead-definitions))
(import
  :gerbil/gambit
  :scheme/base
  :std/coroutine
  :std/format
  :std/generic
  :std/net/request
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
  ;;(try
  (with-input-from-string json read-json))
;;   (catch (e)
;;     (displayln "error parsing json " e))))

(def (hash->str h)
  (let ((results '()))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
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
  (let* ((uri "https://slack.com/api/chat.postMessage")
	 (data (json-object->string
		(hash
		 ("username" from)
		 ("text" message)
		 ("channel" channel))))
	 (results (do-post uri (default-headers) data)))
    (displayln results)))

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
	(displayln .id)))))

(def (msg user message)
  (let-hash (load-config)
    (let ((channel (im-open user)))
      (if channel
	(post channel message .username)
	(displayln "Invalid user: " user)))))


(def (search query)
  (let-hash (load-config)
    (let*
	((uri (format "https://slack.com/api/search.messages?token=~a&count=~a&query=~a" .token 100 query))
	 (results (do-get uri))
	 (myjson (from-json results))
	 (matches (hash-get (hash-get myjson 'messages) 'matches)))

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
	matches))))

(def (users)
  (let-hash (load-config)
    (let* ((uri (format "https://slack.com/api/users.list?token=~a" .token))
	   (results (do-get uri))
	   (myjson (from-json results))
	   (members (hash-ref myjson 'members)))
      members)))

(def (list-users)
  (let ((slack-users (users)))
    (displayln "|Real Name |username| Owner? | Ultra? | Restricted?| team id| Updated| App user?| Profile| id | Time zone|")
    (displayln "|--|-------|-----|-------------|----|---------------|--------|")

    (for-each
      (lambda (u)
;; ((is_owner . #f) (tz . America/Los_Angeles) (is_primary_owner . #f) (is_admin . #f) (name . imartinez) (is_ultra_retricted . #f) (is_restricted . #f) (team_id . T069UQF1S) (is_bot . #f) (updated . 1533241983) (tz_label . PacificDaylight Time) (id . U4MQCQLDU) (color . 8d4b84) (real_name . Izzy Martinez) (deleted . #f) (tz_offset . -25200) (is_app_user . #f) (profile . #<table #614>))
	(let-hash u
	  (displayln
	   "|" .?name
	   "|" .?real_name
	   "|" .?is_primary_owner
	   "|" .?is_ultra_restricted
	   "|" .?is_restricted
	   "|" .?team_id
	   "|" .?updated
	   "|" .?is_app_user
	   "|" (hash->list .profile)
	   "|" .?id
	   "|" .?tz_label
	   "|")))
      slack-users)))

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

(def interactives
  (hash
   ("channels" (hash (description: "Channel list.") (usage: "channels") (count: 0)))
   ("chats" (hash (description: "Chat list.") (usage: "chats") (count: 0)))
   ("ghistory" (hash (description: "Group history.") (usage: "ghistory <group>") (count: 1)))
   ("groups" (hash (description: "Group list.") (usage: "groups") (count: 0)))
   ("id-for-user" (hash (description: "Open chat with user") (usage: "id-for-user user") (count: 1)))
   ("im-open" (hash (description: "Open chat with user") (usage: "chat-open user") (count: 1)))
   ("list-users" (hash (description: "user list.") (usage: "list-users") (count: 0)))
   ("msg" (hash (description: "Send message to user.") (usage: "msg <username> <message>") (count: 2)))
   ("post" (hash (description: "Post IM message to channel.") (usage: "post <channel> <message> <from>") (count: 3)))
   ("search" (hash (description: "Search messages for pattern.") (usage: "searchm <pattern>") (count: 1)))
   ("set-topic" (hash (description: "Set topic on channel") (usage: "set-topic <channel> <topic>") (count: 2)))
   ("user" (hash (description: "Open chat with user") (usage: "user user") (count: 1)))))

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
  (displayln "Usage: datadog <verb>")
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
	   (myjson (from-json results)))
      (displayln results))))

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
    config))

(def (default-headers)
  (let-hash (load-config)
    [
     ["Accept" :: "*/*"]
     ["Content-type" :: "application/json"]
     ["Authorization" :: (format "Bearer ~a" .token) ]
     ]))
