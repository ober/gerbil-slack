;; -*- Gerbil -*-
;; Slack CLI entry point (stub for Phase 1)

(import
  :std/format
  :std/sugar
  :ober/slack/config
  :ober/slack/http
  :ober/slack/api/auth)

(export main)

(def (main . args)
  (if (null? args)
    (begin
      (displayln (format "slack ~a" version))
      (displayln "Usage: slack <command> [args...]")
      (displayln "Commands: auth-test, config-setup")
      (exit 2))
    (let ((verb (car args)))
      (cond
       ((string=? verb "auth-test")
        (let ((result (auth-test)))
          (let-hash result
            (displayln (format "Team: ~a | User: ~a | URL: ~a"
                               .?team .?user .?url)))))
       ((string=? verb "config-setup")
        (config-setup!))
       (else
        (displayln "Unknown command: " verb)
        (exit 2))))))
