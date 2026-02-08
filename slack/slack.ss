;; -*- Gerbil -*-
;; Slack CLI entry point

(import
  :std/sugar
  :ober/slack/config
  :ober/slack/cli/format
  :ober/slack/cli/commands)

(export main)

(def version "0.2.0")

(def (main . args)
  (with-catch
    (lambda (e)
      (displayln (red (format-error e)))
      (exit 1))
    (lambda ()
      ;; Handle global flags
      (let-values (((clean-args json? no-color? token) (extract-global-flags args)))
        (when no-color? (set-color! #f))
        (when token (setenv "SLACK_TOKEN" token))
        (if (null? clean-args)
          (show-help)
          (run-cli clean-args))))))

(def (extract-global-flags args)
  "Extract --json, --no-color, --token from args. Returns (values clean-args json? no-color? token)."
  (let loop ((rest args) (clean []) (json? #f) (no-color? #f) (token #f))
    (cond
     ((null? rest) (values (reverse clean) json? no-color? token))
     ((string=? (car rest) "--json")
      (loop (cdr rest) clean #t no-color? token))
     ((string=? (car rest) "--no-color")
      (loop (cdr rest) clean json? #t token))
     ((and (string=? (car rest) "--token") (pair? (cdr rest)))
      (loop (cddr rest) clean json? no-color? (cadr rest)))
     ((string=? (car rest) "--version")
      (displayln "slack " version)
      (exit 0))
     (else
      (loop (cdr rest) (cons (car rest) clean) json? no-color? token)))))

(def (show-help)
  (displayln (bold "slack") " v" version " — Slack CLI")
  (displayln)
  (displayln "Usage: slack <command> [subcommand] [args...]")
  (displayln)
  (displayln (bold "Commands:"))
  (displayln "  auth       Authentication (test, setup)")
  (displayln "  channels   Channel management (list, info, create, ...)")
  (displayln "  messages   Message operations (history, send, edit, delete, ...)")
  (displayln "  users      User operations (list, info, status, ...)")
  (displayln "  dm         Direct messages (send, history, open)")
  (displayln "  react      Reactions (add, remove, list)")
  (displayln "  pin        Pins (add, remove, list)")
  (displayln "  files      File operations (upload, list, info, delete, download)")
  (displayln "  search     Search (messages, files)")
  (displayln "  emoji      Emoji (list)")
  (displayln "  team       Team info")
  (displayln "  reminders  Reminders (add, list, complete, delete)")
  (displayln "  dnd        Do Not Disturb (snooze, end, info)")
  (displayln "  listen     Stream real-time events")
  (displayln)
  (displayln (bold "Global flags:"))
  (displayln "  --token TOKEN  Override config token")
  (displayln "  --no-color     Disable ANSI colors")
  (displayln "  --version      Show version"))

(def (format-error e)
  (cond
   ((error-exception? e)
    (error-exception-message e))
   ((string? e) e)
   (else
    (call-with-output-string
      (lambda (p) (display-exception e p))))))
