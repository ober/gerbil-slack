;; -*- Gerbil -*-
;; Configuration loading and token storage

(import
  :gerbil/gambit
  :std/crypto/cipher
  :std/error
  :std/format
  :std/misc/ports
  :std/sugar
  :std/text/base64
  :std/text/json)

(export #t)

(def config-file "~/.slack.json")
(def program-name "slack")
(def version "0.10")

;; Cache for loaded config (populated on first load)
(def *config-cache* #f)

;;;; Config Loading

(def (config-path)
  "Return the config file path, respecting SLACK_CONFIG env var."
  (or (getenv "SLACK_CONFIG" #f)
      config-file))

(def (load-config)
  "Load and return the config hash, caching after first call.
   Supports SLACK_TOKEN env var override."
  (or *config-cache*
      (let ((cfg (load-config!)))
        (set! *config-cache* cfg)
        cfg)))

(def (load-config!)
  "Load config from disk without caching."
  (let* ((path (config-path))
         (expanded (path-expand path)))
    (unless (file-exists? expanded)
      (error (format "Config file not found: ~a" path)))
    (let ((config (parameterize ((read-json-key-as-symbol? #t))
                    (call-with-input-file expanded read-json))))
      (unless (hash-table? config)
        (error (format "Invalid config format in ~a" path)))

      ;; Decrypt token if secrets are present
      (when (hash-get config 'secrets)
        (let* ((secrets-str (hash-ref config 'secrets))
               (secrets (parameterize ((read-json-key-as-symbol? #t))
                          (call-with-input-string
                           (bytes->string (base64-decode secrets-str))
                           read-json))))
          (when (hash-table? secrets)
            (let-hash secrets
              (when (and .?key .?iv .?password)
                (hash-put! config 'token
                           (decrypt-token .key .iv .password)))))))

      ;; Allow SLACK_TOKEN env var to override config file token
      (let ((env-token (getenv "SLACK_TOKEN" #f)))
        (when env-token
          (hash-put! config 'token env-token)))

      ;; Validate required fields
      (unless (hash-get config 'token)
        (error "No token found. Set SLACK_TOKEN env var or add 'token' to config."))

      config)))

(def (save-config cfg)
  "Write config hash to disk as JSON."
  (let* ((path (config-path))
         (expanded (path-expand path))
         (temp-path (string-append expanded ".tmp")))
    (call-with-output-file temp-path
      (lambda (p)
        (parameterize ((current-output-port p))
          (write-json cfg))))
    (rename-file temp-path expanded)))

(def (reset-config-cache!)
  "Clear the config cache, forcing reload on next access."
  (set! *config-cache* #f))

;;;; Token Encryption

(def (encrypt-token token)
  "Encrypt a token string. Returns hash with key, iv, password (all base64)."
  (let* ((cipher (make-aes-256-ctr-cipher))
         (iv (random-bytes (cipher-iv-length cipher)))
         (key (random-bytes (cipher-key-length cipher)))
         (encrypted (encrypt cipher key iv (string->bytes token))))
    (hash ("key" (u8vector->base64-string key))
          ("iv" (u8vector->base64-string iv))
          ("password" (u8vector->base64-string encrypted)))))

(def (decrypt-token key-b64 iv-b64 password-b64)
  "Decrypt a token from base64-encoded key, iv, and encrypted password."
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key-b64)
    (base64-string->u8vector iv-b64)
    (base64-string->u8vector password-b64))))

;;;; Interactive Setup

(def (config-setup!)
  "Interactive first-run setup. Prompts for token, tests it, saves config."
  (display "Enter your Slack Bot Token (xoxb-...): ")
  (force-output)
  (let ((token (read-line (current-input-port))))
    (when (or (not token) (eof-object? token) (string=? token ""))
      (error "No token provided."))

    (display "Encrypt token? (y/n): ")
    (force-output)
    (let* ((encrypt? (let ((ans (read-line (current-input-port))))
                       (and ans (> (string-length ans) 0)
                            (char=? (string-ref ans 0) #\y))))
           (config (if encrypt?
                     (let ((secrets (encrypt-token token)))
                       (hash ("secrets" (base64-encode
                                         (string->bytes
                                          (json-object->string secrets))))))
                     (hash ("token" token)))))
      (save-config config)
      (displayln "Config saved to " (config-path))
      config)))

;;;; Config Accessors

(def (config-token)
  "Get the token from config."
  (hash-ref (load-config) 'token))

(def (config-ref key (default #f))
  "Get a value from config with optional default."
  (let ((v (hash-get (load-config) key)))
    (or v default)))
