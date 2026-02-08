;; -*- Gerbil -*-
;; Slack files API methods

(import
  :std/net/request
  :std/sugar
  :std/text/json
  :ober/slack/config
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (files-upload file-path
                   channels: (channels #f)
                   title: (title #f)
                   initial-comment: (initial-comment #f)
                   thread-ts: (thread-ts #f)
                   token: (token #f))
  "Upload a file. Returns file-info struct."
  ;; files.upload uses multipart/form-data, handled separately
  (let* ((tok (or token (config-token)))
         (params (append (if channels (list (cons "channels" channels)) [])
                         (if title (list (cons "title" title)) [])
                         (if initial-comment (list (cons "initial_comment" initial-comment)) [])
                         (if thread-ts (list (cons "thread_ts" thread-ts)) [])))
         (query (if (null? params)
                  ""
                  (string-append "?" (params->query params))))
         (url (string-append slack-api-base "/files.upload" query))
         (file-content (call-with-input-file file-path
                         (lambda (p)
                           (let loop ((acc []))
                             (let ((b (read-u8 p)))
                               (if (eof-object? b)
                                 (list->u8vector (reverse acc))
                                 (loop (cons b acc))))))))
         (resp (http-post url
                          headers: `(("Authorization" . ,(string-append "Bearer " tok)))
                          data: file-content))
         (body (parameterize ((read-json-key-as-symbol? #t))
                 (call-with-input-string (request-text resp) read-json)))
         (file (and (hash-table? body) (hash-get body 'file))))
    (if file (json->file-info file) (error "Upload failed"))))

(def (files-delete file-id token: (token #f))
  "Delete a file."
  (slack-post "files.delete"
              (list (cons "file" file-id))
              token: token)
  #t)

(def (files-info file-id token: (token #f))
  "Get info about a file. Returns file-info struct."
  (let* ((resp (slack-api "files.info"
                          (list (cons "file" file-id))
                          token: token))
         (file (hash-get resp 'file)))
    (if file (json->file-info file) (error "No file in response"))))

(def (files-list channel: (channel #f)
                 user: (user #f)
                 types: (types #f)
                 count: (count 100)
                 token: (token #f))
  "List files. Returns list of file-info structs."
  (let* ((params (append (list (cons "count" (number->string count)))
                         (if channel (list (cons "channel" channel)) [])
                         (if user (list (cons "user" user)) [])
                         (if types (list (cons "types" types)) [])))
         (resp (slack-api "files.list" params token: token))
         (files (hash-get resp 'files)))
    (if files (map json->file-info files) [])))

(def (file-download file-url dest-path token: (token #f))
  "Download a private file. Returns #t on success."
  (let* ((tok (or token (config-token)))
         (resp (http-get file-url
                         headers: `(("Authorization" . ,(string-append "Bearer " tok))))))
    (call-with-output-file dest-path
      (lambda (p)
        (let ((bytes (request-response-bytes resp)))
          (write-subu8vector bytes 0 (u8vector-length bytes) p))))
    #t))
