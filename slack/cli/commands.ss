;; -*- Gerbil -*-
;; CLI command definitions and dispatch

(import
  :std/format
  :std/getopt
  :std/sugar
  :std/text/json
  :ober/slack/types
  :ober/slack/config
  :ober/slack/http
  :ober/slack/api/auth
  :ober/slack/api/users
  :ober/slack/api/conversations
  :ober/slack/api/chat
  :ober/slack/api/reactions
  :ober/slack/api/files
  :ober/slack/api/pins
  :ober/slack/api/search
  :ober/slack/api/emoji
  :ober/slack/api/team
  :ober/slack/api/reminders
  :ober/slack/api/dnd
  :ober/slack/api/usergroups
  :ober/slack/api/bookmarks
  :ober/slack/events
  :ober/slack/socket
  :ober/slack/cache
  :ober/slack/markdown
  :ober/slack/cli/format)

(export #t)

;;;; Command Definitions

(def auth-cmd
  (command 'auth help: "Authentication commands"
    (rest-arguments 'args help: "subcommand")))

(def channels-cmd
  (command 'channels help: "Channel management"
    (rest-arguments 'args help: "subcommand and args")))

(def messages-cmd
  (command 'messages help: "Message operations"
    (rest-arguments 'args help: "subcommand and args")))

(def users-cmd
  (command 'users help: "User operations"
    (rest-arguments 'args help: "subcommand and args")))

(def dm-cmd
  (command 'dm help: "Direct messages"
    (rest-arguments 'args help: "subcommand and args")))

(def react-cmd
  (command 'react help: "Reactions"
    (rest-arguments 'args help: "subcommand and args")))

(def pin-cmd
  (command 'pin help: "Pins"
    (rest-arguments 'args help: "subcommand and args")))

(def files-cmd
  (command 'files help: "File operations"
    (rest-arguments 'args help: "subcommand and args")))

(def search-cmd
  (command 'search help: "Search messages and files"
    (rest-arguments 'args help: "subcommand and args")))

(def emoji-cmd
  (command 'emoji help: "Emoji"
    (rest-arguments 'args help: "subcommand and args")))

(def team-cmd
  (command 'team help: "Team info"
    (rest-arguments 'args help: "subcommand and args")))

(def reminders-cmd
  (command 'reminders help: "Reminders"
    (rest-arguments 'args help: "subcommand and args")))

(def dnd-cmd
  (command 'dnd help: "Do Not Disturb"
    (rest-arguments 'args help: "subcommand and args")))

(def listen-cmd
  (command 'listen help: "Stream real-time events to stdout"
    (rest-arguments 'args help: "args")))

;;;; Top-Level CLI

(def all-commands
  [auth-cmd channels-cmd messages-cmd users-cmd dm-cmd
   react-cmd pin-cmd files-cmd search-cmd emoji-cmd
   team-cmd reminders-cmd dnd-cmd listen-cmd])

(def (run-cli args)
  "Main CLI entry point."
  (apply call-with-getopt
    dispatch-command
    args
    program: "slack"
    help: "Slack CLI — interact with Slack from the command line"
    all-commands))

(def (dispatch-command cmd opt)
  "Dispatch to the appropriate command handler."
  (let-hash opt
    ;; Note: token override handled by main entry point
    ;; Dispatch
    (case cmd
      ((auth) (handle-auth .args))
      ((channels) (handle-channels .args))
      ((messages) (handle-messages .args))
      ((users) (handle-users .args))
      ((dm) (handle-dm .args))
      ((react) (handle-react .args))
      ((pin) (handle-pin .args))
      ((files) (handle-files .args))
      ((search) (handle-search .args))
      ((emoji) (handle-emoji .args))
      ((team) (handle-team .args))
      ((reminders) (handle-reminders .args))
      ((dnd) (handle-dnd .args))
      ((listen) (handle-listen .args))
      (else (displayln "Unknown command: " cmd)))))

;;;; Auth Commands

(def (handle-auth args)
  (let ((sub (and (pair? args) (car args))))
    (case (and sub (string->symbol sub))
      ((test)
       (let ((result (auth-test)))
         (if result
           (begin
             (displayln (bold "Authentication successful"))
             (displayln "Team: " (green (or (hash-get result 'team) "?")))
             (displayln "User: " (cyan (or (hash-get result 'user) "?")))
             (displayln "URL:  " (or (hash-get result 'url) "?")))
           (displayln (red "Authentication failed")))))
      ((setup)
       (config-setup!)
       (displayln (green "Configuration saved")))
      (else
       (displayln "Usage: slack auth <test|setup>")))))

;;;; Channel Commands

(def (handle-channels args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((list)
       (let ((channels (conversations-list)))
         (format-table
          ["ID" "Name" "Members" "Topic"]
          (map (lambda (ch)
                 (list (channel-id ch)
                       (string-append (if (channel-is-private ch) "🔒" "#")
                                      (or (channel-name ch) ""))
                       (or (channel-num-members ch) 0)
                       (truncate-text (or (channel-topic ch) "") 40)))
               channels))))
      ((info)
       (when (null? rest) (error "Usage: slack channels info CHANNEL"))
       (let ((ch (conversations-info (car rest))))
         (displayln (bold "Channel: ") (format-channel ch))
         (displayln "  ID:       " (channel-id ch))
         (displayln "  Members:  " (or (channel-num-members ch) "?"))
         (displayln "  Purpose:  " (or (channel-purpose ch) ""))
         (displayln "  Created:  " (format-timestamp (channel-created ch)))
         (displayln "  Archived: " (if (channel-is-archived ch) "yes" "no"))))
      ((create)
       (when (null? rest) (error "Usage: slack channels create NAME [--private]"))
       (let ((ch (conversations-create (car rest)
                   is-private: (member "--private" rest))))
         (displayln (green "Created: ") (format-channel ch))))
      ((topic)
       (when (< (length rest) 2) (error "Usage: slack channels topic CHANNEL TOPIC"))
       (conversations-set-topic (car rest) (cadr rest))
       (displayln (green "Topic set")))
      ((purpose)
       (when (< (length rest) 2) (error "Usage: slack channels purpose CHANNEL PURPOSE"))
       (conversations-set-purpose (car rest) (cadr rest))
       (displayln (green "Purpose set")))
      ((join)
       (when (null? rest) (error "Usage: slack channels join CHANNEL"))
       (conversations-join (car rest))
       (displayln (green "Joined")))
      ((leave)
       (when (null? rest) (error "Usage: slack channels leave CHANNEL"))
       (conversations-leave (car rest))
       (displayln (green "Left")))
      ((members)
       (when (null? rest) (error "Usage: slack channels members CHANNEL"))
       (let ((member-ids (conversations-members (car rest))))
         (for-each displayln member-ids)))
      ((invite)
       (when (< (length rest) 2) (error "Usage: slack channels invite CHANNEL USER"))
       (conversations-invite (car rest) (cadr rest))
       (displayln (green "Invited")))
      ((archive)
       (when (null? rest) (error "Usage: slack channels archive CHANNEL"))
       (conversations-archive (car rest))
       (displayln (green "Archived")))
      (else
       (displayln "Usage: slack channels <list|info|create|topic|purpose|join|leave|members|invite|archive>")))))

;;;; Message Commands

(def (handle-messages args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((history)
       (when (null? rest) (error "Usage: slack messages history CHANNEL [--limit N]"))
       (let* ((channel (car rest))
              (limit (get-flag-value rest "--limit" 20 string->number))
              (msgs (conversations-history channel limit: limit)))
         (format-messages (reverse msgs))))
      ((send)
       (when (< (length rest) 2) (error "Usage: slack messages send CHANNEL TEXT [--thread TS]"))
       (let* ((channel (car rest))
              (text (cadr rest))
              (thread (get-flag-value rest "--thread" #f)))
         (let ((ts (if thread
                      (chat-post-message channel text thread-ts: thread)
                      (chat-post-message channel text))))
           (displayln (green "Message sent") " (ts: " (or ts "") ")"))))
      ((edit)
       (when (< (length rest) 3) (error "Usage: slack messages edit CHANNEL TS TEXT"))
       (chat-update (car rest) (cadr rest) (caddr rest))
       (displayln (green "Message edited")))
      ((delete)
       (when (< (length rest) 2) (error "Usage: slack messages delete CHANNEL TS"))
       (chat-delete (car rest) (cadr rest))
       (displayln (green "Message deleted")))
      ((thread)
       (when (< (length rest) 2) (error "Usage: slack messages thread CHANNEL TS [--limit N]"))
       (let* ((channel (car rest))
              (ts (cadr rest))
              (limit (get-flag-value rest "--limit" 20 string->number))
              (msgs (conversations-replies channel ts limit: limit)))
         (format-messages msgs)))
      ((link)
       (when (< (length rest) 2) (error "Usage: slack messages link CHANNEL TS"))
       (let ((url (chat-get-permalink (car rest) (cadr rest))))
         (displayln url)))
      ((schedule)
       (when (< (length rest) 3) (error "Usage: slack messages schedule CHANNEL TEXT TIME"))
       (let ((msg (chat-schedule-message (car rest) (cadr rest) (caddr rest))))
         (displayln (green "Message scheduled"))))
      (else
       (displayln "Usage: slack messages <history|send|edit|delete|thread|link|schedule>")))))

;;;; User Commands

(def (handle-users args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((list)
       (let ((users (users-list)))
         (format-table
          ["ID" "Name" "Real Name" "Status"]
          (map (lambda (u)
                 (list (user-id u)
                       (or (user-name u) "")
                       (or (user-real-name u) "")
                       (or (user-status-text u) "")))
               users))))
      ((info)
       (when (null? rest) (error "Usage: slack users info USER"))
       (let ((u (users-info (car rest))))
         (displayln (format-user u))
         (displayln "  ID:    " (user-id u))
         (displayln "  Email: " (or (user-email u) ""))
         (displayln "  TZ:    " (or (user-tz u) ""))
         (displayln "  Bot:   " (if (user-is-bot u) "yes" "no"))
         (displayln "  Admin: " (if (user-is-admin u) "yes" "no"))))
      ((presence)
       (when (null? rest) (error "Usage: slack users presence USER"))
       (let ((p (users-get-presence (car rest))))
         (displayln "Presence: " (if p (green (symbol->string p)) (dim "unknown")))))
      ((status)
       (when (< (length rest) 2) (error "Usage: slack users status TEXT EMOJI"))
       (users-set-status (car rest) (cadr rest))
       (displayln (green "Status set")))
      ((active)
       (users-set-presence 'auto)
       (displayln (green "Set to active")))
      ((away)
       (users-set-presence 'away)
       (displayln (green "Set to away")))
      (else
       (displayln "Usage: slack users <list|info|presence|status|active|away>")))))

;;;; DM Commands

(def (handle-dm args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((send)
       (when (< (length rest) 2) (error "Usage: slack dm send USER TEXT"))
       (let ((ch-id (conversations-open users: (car rest))))
         (chat-post-message ch-id (cadr rest))
         (displayln (green "DM sent"))))
      ((history)
       (when (null? rest) (error "Usage: slack dm history USER [--limit N]"))
       (let* ((ch-id (conversations-open users: (car rest)))
              (limit (get-flag-value rest "--limit" 20 string->number))
              (msgs (conversations-history ch-id limit: limit)))
         (format-messages (reverse msgs))))
      ((open)
       (when (null? rest) (error "Usage: slack dm open USER"))
       (let ((ch-id (conversations-open users: (car rest))))
         (displayln "DM channel: " ch-id)))
      (else
       (displayln "Usage: slack dm <send|history|open>")))))

;;;; Reaction Commands

(def (handle-react args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((add)
       (when (< (length rest) 3) (error "Usage: slack react add CHANNEL TS EMOJI"))
       (reactions-add (car rest) (cadr rest) (caddr rest))
       (displayln (green "Reaction added")))
      ((remove)
       (when (< (length rest) 3) (error "Usage: slack react remove CHANNEL TS EMOJI"))
       (reactions-remove (car rest) (cadr rest) (caddr rest))
       (displayln (green "Reaction removed")))
      ((list)
       (when (< (length rest) 2) (error "Usage: slack react list CHANNEL TS"))
       (let ((reactions (reactions-get (car rest) (cadr rest))))
         (for-each
           (lambda (r)
             (displayln ":" (reaction-name r) ": (" (reaction-count r) ")"))
           reactions)))
      (else
       (displayln "Usage: slack react <add|remove|list>")))))

;;;; Pin Commands

(def (handle-pin args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((add)
       (when (< (length rest) 2) (error "Usage: slack pin add CHANNEL TS"))
       (pins-add (car rest) (cadr rest))
       (displayln (green "Pinned")))
      ((remove)
       (when (< (length rest) 2) (error "Usage: slack pin remove CHANNEL TS"))
       (pins-remove (car rest) (cadr rest))
       (displayln (green "Unpinned")))
      ((list)
       (when (null? rest) (error "Usage: slack pin list CHANNEL"))
       (let ((pins (pins-list (car rest))))
         (for-each (lambda (p) (displayln p)) pins)))
      (else
       (displayln "Usage: slack pin <add|remove|list>")))))

;;;; File Commands

(def (handle-files args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((upload)
       (when (null? rest) (error "Usage: slack files upload PATH [--channel CH] [--title T]"))
       (let* ((path (car rest))
              (channel (get-flag-value rest "--channel" #f))
              (title (get-flag-value rest "--title" #f)))
         (let ((f (files-upload path channels: channel title: title)))
           (displayln (green "File uploaded: ") (file-info-name f)))))
      ((list)
       (let* ((channel (get-flag-value rest "--channel" #f))
              (user (get-flag-value rest "--user" #f))
              (files (files-list channel: channel user: user)))
         (format-table
          ["ID" "Name" "Type" "Size"]
          (map (lambda (f)
                 (list (file-info-id f)
                       (or (file-info-name f) "")
                       (or (file-info-mimetype f) "")
                       (or (file-info-size f) 0)))
               files))))
      ((info)
       (when (null? rest) (error "Usage: slack files info FILE_ID"))
       (let ((f (files-info (car rest))))
         (displayln "File: " (bold (or (file-info-name f) "?")))
         (displayln "  ID:    " (file-info-id f))
         (displayln "  Title: " (or (file-info-title f) ""))
         (displayln "  Type:  " (or (file-info-mimetype f) ""))
         (displayln "  Size:  " (or (file-info-size f) 0))
         (displayln "  User:  " (or (file-info-user f) ""))))
      ((delete)
       (when (null? rest) (error "Usage: slack files delete FILE_ID"))
       (files-delete (car rest))
       (displayln (green "File deleted")))
      ((download)
       (when (null? rest) (error "Usage: slack files download FILE_ID [DEST]"))
       (let* ((f (files-info (car rest)))
              (url (file-info-url-private f))
              (dest (if (> (length rest) 1)
                      (cadr rest)
                      (or (file-info-name f) "download"))))
         (unless url (error "No download URL for file"))
         (file-download url dest)
         (displayln (green "Downloaded to: ") dest)))
      (else
       (displayln "Usage: slack files <upload|list|info|delete|download>")))))

;;;; Search Commands

(def (handle-search args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((messages)
       (when (null? rest) (error "Usage: slack search messages QUERY"))
       (let* ((count (get-flag-value rest "--count" 20 string->number))
              (results (search-messages (car rest) count: count)))
         (for-each (lambda (m) (displayln (message-summary m))) results)))
      ((files)
       (when (null? rest) (error "Usage: slack search files QUERY"))
       (let* ((count (get-flag-value rest "--count" 20 string->number))
              (results (search-files (car rest) count: count)))
         (for-each (lambda (f) (displayln (file-info-name f) " (" (file-info-id f) ")"))
                   results)))
      (else
       (displayln "Usage: slack search <messages|files>")))))

;;;; Emoji Command

(def (handle-emoji args)
  (let ((sub (and (pair? args) (car args))))
    (case (and sub (string->symbol sub))
      ((list)
       (let ((emojis (emoji-list)))
         (hash-for-each (lambda (k v) (displayln ":" k ": → " v)) emojis)))
      (else
       (displayln "Usage: slack emoji list")))))

;;;; Team Command

(def (handle-team args)
  (let ((sub (and (pair? args) (car args))))
    (case (and sub (string->symbol sub))
      ((info)
       (let ((t (team-info)))
         (displayln (bold "Team: ") (team-name t))
         (displayln "  ID:     " (team-id t))
         (displayln "  Domain: " (or (team-domain t) ""))))
      (else
       (displayln "Usage: slack team info")))))

;;;; Reminder Commands

(def (handle-reminders args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((add)
       (when (< (length rest) 2) (error "Usage: slack reminders add TEXT TIME"))
       (let ((r (reminders-add (car rest) (cadr rest))))
         (displayln (green "Reminder created: ") (reminder-id r))))
      ((list)
       (let ((rs (reminders-list)))
         (format-table
          ["ID" "Text" "Complete"]
          (map (lambda (r)
                 (list (reminder-id r)
                       (or (reminder-text r) "")
                       (if (reminder-complete-ts r) "✓" "")))
               rs))))
      ((complete)
       (when (null? rest) (error "Usage: slack reminders complete ID"))
       (reminders-complete (car rest))
       (displayln (green "Reminder completed")))
      ((delete)
       (when (null? rest) (error "Usage: slack reminders delete ID"))
       (reminders-delete (car rest))
       (displayln (green "Reminder deleted")))
      (else
       (displayln "Usage: slack reminders <add|list|complete|delete>")))))

;;;; DND Commands

(def (handle-dnd args)
  (let ((sub (and (pair? args) (car args)))
        (rest (if (pair? args) (cdr args) [])))
    (case (and sub (string->symbol sub))
      ((snooze)
       (when (null? rest) (error "Usage: slack dnd snooze MINUTES"))
       (dnd-set-snooze (string->number (car rest)))
       (displayln (green "DND enabled")))
      ((end)
       (dnd-end-snooze)
       (displayln (green "DND disabled")))
      ((info)
       (let ((info (dnd-info)))
         (displayln "DND Info: " info)))
      (else
       (displayln "Usage: slack dnd <snooze|end|info>")))))

;;;; Listen Command

(def (handle-listen args)
  (let ((app-token (or (getenv "SLACK_APP_TOKEN" #f)
                       (config-ref "app_token"))))
    (unless app-token
      (error "No app token — set SLACK_APP_TOKEN or configure app_token"))
    (displayln "Connecting to Socket Mode...")
    (on-event '*
      (lambda (ev)
        (displayln (format "~a: ~a"
                           (slack-event-type ev)
                           (let ((data (slack-event-data ev)))
                             (if (hash-table? data)
                               (json-object->string data)
                               data))))))
    (socket-mode-start! app-token)
    ;; Block forever
    (let loop () (thread-sleep! 3600) (loop))))

;;;; Helpers

(def (get-flag-value args flag-name default (converter #f))
  "Extract --flag VALUE from an args list."
  (let loop ((rest args))
    (cond
     ((null? rest) default)
     ((and (pair? (cdr rest))
           (string=? (car rest) flag-name))
      (let ((val (cadr rest)))
        (if converter (converter val) val)))
     (else (loop (cdr rest))))))
