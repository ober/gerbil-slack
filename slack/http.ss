;; -*- Gerbil -*-
;; HTTP client for Slack API — auth, rate limiting, pagination

(import
  :std/error
  :std/format
  :std/net/request
  :std/sugar
  :std/text/json
  :ober/slack/config)

(export #t)

(def slack-api-base "https://slack.com/api")

;;;; Error Type

(defclass slack-error (method code message)
  transparent: #t)

(def (raise-slack-error method code message)
  (raise (make-slack-error method: method code: code message: message)))

;;;; Core API Caller

(def (slack-api method
               (params '())
               method-type: (method-type 'get)
               token: (token #f))
  "Call a Slack API method. Returns parsed JSON hash with symbol keys.
   method: Slack API method name (e.g. \"auth.test\")
   params: alist of parameters
   method-type: 'get or 'post
   token: override token (defaults to config token)"
  (let* ((tok (or token (config-token)))
         (url (string-append slack-api-base "/" method))
         (headers `(("Authorization" . ,(string-append "Bearer " tok))
                    ("Content-Type" . "application/json; charset=utf-8"))))
    (let ((resp (case method-type
                  ((get)
                   (let ((full-url (if (null? params)
                                     url
                                     (string-append url "?" (params->query params)))))
                     (http-get full-url headers: headers)))
                  ((post)
                   (http-post url
                              headers: headers
                              data: (json-object->string (params->hash params))))
                  (else
                   (error (format "Unknown method type: ~a" method-type))))))
      (handle-response method resp))))

(def (slack-post method
                 (params '())
                 token: (token #f))
  "Convenience wrapper for POST API calls."
  (slack-api method params method-type: 'post token: token))

;;;; Response Handling

(def (handle-response method resp)
  "Parse response, handle rate limiting, check for errors."
  (let ((status (request-status resp)))
    ;; Rate limited — wait and retry
    (when (= status 429)
      (let* ((headers (request-headers resp))
             (retry-after (or (alist-get headers "Retry-After") "1"))
             (wait-secs (string->number retry-after)))
        (thread-sleep! (or wait-secs 1))
        ;; The caller should retry — we signal this
        (raise-slack-error method "rate_limited"
                           (format "Rate limited. Retry after ~a seconds." retry-after))))

    ;; HTTP error
    (unless (and (>= status 200) (< status 300))
      (raise-slack-error method "http_error"
                         (format "HTTP ~a: ~a" status (request-text resp))))

    ;; Parse JSON response
    (let* ((text (request-text resp))
           (body (if (and text (> (string-length text) 0))
                   (parameterize ((read-json-key-as-symbol? #t))
                     (call-with-input-string text read-json))
                   (hash))))
      (unless (hash-table? body)
        (raise-slack-error method "parse_error" "Response is not a JSON object"))

      ;; Check Slack API ok field
      (let-hash body
        (unless .?ok
          (raise-slack-error method
                             (or .?error "unknown_error")
                             (or .?error "API call failed"))))
      body)))

;;;; Pagination

(def (slack-api-paginated method
                          (params '())
                          key: (key #f)
                          method-type: (method-type 'get)
                          token: (token #f)
                          limit: (limit 200))
  "Call a paginated Slack API method, collecting all results.
   key: the response field containing the list (symbol, e.g. 'members, 'channels)
   Returns the full list of items."
  (let loop ((cursor #f)
             (acc []))
    (let* ((page-params (append params
                                (list (cons "limit" (number->string limit)))
                                (if cursor
                                  (list (cons "cursor" cursor))
                                  [])))
           (resp (slack-api method page-params
                            method-type: method-type
                            token: token))
           (items (if key (hash-get resp key) resp))
           (new-acc (if (list? items)
                      (append acc items)
                      acc))
           (metadata (hash-get resp 'response_metadata))
           (next-cursor (and metadata
                             (hash-table? metadata)
                             (let ((c (hash-get metadata 'next_cursor)))
                               (and c (string? c) (> (string-length c) 0) c)))))
      (if next-cursor
        (loop next-cursor new-acc)
        new-acc))))

;;;; Helpers

(def (params->query params)
  "Convert alist params to URL query string."
  (string-join
   (map (lambda (p)
          (string-append (url-encode (car p)) "=" (url-encode (cdr p))))
        params)
   "&"))

(def (params->hash params)
  "Convert alist params to hash table for JSON serialization."
  (let ((h (make-hash-table)))
    (for-each (lambda (p) (hash-put! h (car p) (cdr p))) params)
    h))

(def (url-encode s)
  "Percent-encode a string for URL query parameters."
  (call-with-output-string
   (lambda (out)
     (for-each
      (lambda (c)
        (cond
         ((or (char-alphabetic? c) (char-numeric? c)
              (memv c '(#\- #\_ #\. #\~)))
          (write-char c out))
         (else
          (let* ((b (char->integer c))
                 (hex (number->string b 16)))
            (display "%" out)
            (when (< b 16) (display "0" out))
            (display (string-upcase hex) out)))))
      (string->list s)))))

(def (string-join strs sep)
  "Join a list of strings with separator."
  (if (null? strs) ""
    (let loop ((rest (cdr strs))
               (acc (car strs)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

(def (alist-get alist key)
  "Get value from an alist by string key (case-insensitive)."
  (let ((key-lower (string-downcase key)))
    (let loop ((rest alist))
      (if (null? rest) #f
        (let ((pair (car rest)))
          (if (string=? (string-downcase (car pair)) key-lower)
            (cdr pair)
            (loop (cdr rest))))))))
