;; -*- Gerbil -*-
;; Slack search API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (search-messages query
                      sort: (sort #f)
                      sort-dir: (sort-dir #f)
                      count: (count 20)
                      token: (token #f))
  "Search messages. Returns list of message structs."
  (let* ((params (append (list (cons "query" query)
                               (cons "count" (number->string count)))
                         (if sort (list (cons "sort" sort)) [])
                         (if sort-dir (list (cons "sort_dir" sort-dir)) [])))
         (resp (slack-api "search.messages" params token: token))
         (messages (hash-get resp 'messages))
         (matches (and messages (hash-table? messages) (hash-get messages 'matches))))
    (if matches (map json->message matches) [])))

(def (search-files query
                   sort: (sort #f)
                   sort-dir: (sort-dir #f)
                   count: (count 20)
                   token: (token #f))
  "Search files. Returns list of file-info structs."
  (let* ((params (append (list (cons "query" query)
                               (cons "count" (number->string count)))
                         (if sort (list (cons "sort" sort)) [])
                         (if sort-dir (list (cons "sort_dir" sort-dir)) [])))
         (resp (slack-api "search.files" params token: token))
         (files (hash-get resp 'files))
         (matches (and files (hash-table? files) (hash-get files 'matches))))
    (if matches (map json->file-info matches) [])))

(def (search-all query
                 sort: (sort #f)
                 sort-dir: (sort-dir #f)
                 count: (count 20)
                 token: (token #f))
  "Search messages and files. Returns hash with 'messages and 'files keys."
  (let* ((params (append (list (cons "query" query)
                               (cons "count" (number->string count)))
                         (if sort (list (cons "sort" sort)) [])
                         (if sort-dir (list (cons "sort_dir" sort-dir)) [])))
         (resp (slack-api "search.all" params token: token)))
    resp))
