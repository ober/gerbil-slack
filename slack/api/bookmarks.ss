;; -*- Gerbil -*-
;; Slack bookmarks API methods

(import
  :std/sugar
  :ober/slack/types
  :ober/slack/http)

(export #t)

(def (bookmarks-add channel-id title type link
                    emoji: (emoji #f)
                    token: (token #f))
  "Add a bookmark to a channel. Returns bookmark struct."
  (let* ((params (append (list (cons "channel_id" channel-id)
                               (cons "title" title)
                               (cons "type" type)
                               (cons "link" link))
                         (if emoji (list (cons "emoji" emoji)) [])))
         (resp (slack-post "bookmarks.add" params token: token))
         (bk (hash-get resp 'bookmark)))
    (if bk (json->bookmark bk) (error "No bookmark in response"))))

(def (bookmarks-edit bookmark-id channel-id
                     title: (title #f)
                     link: (link #f)
                     emoji: (emoji #f)
                     token: (token #f))
  "Edit a bookmark. Returns bookmark struct."
  (let* ((params (append (list (cons "bookmark_id" bookmark-id)
                               (cons "channel_id" channel-id))
                         (if title (list (cons "title" title)) [])
                         (if link (list (cons "link" link)) [])
                         (if emoji (list (cons "emoji" emoji)) [])))
         (resp (slack-post "bookmarks.edit" params token: token))
         (bk (hash-get resp 'bookmark)))
    (if bk (json->bookmark bk) (error "No bookmark in response"))))

(def (bookmarks-remove bookmark-id channel-id token: (token #f))
  "Remove a bookmark."
  (slack-post "bookmarks.remove"
              (list (cons "bookmark_id" bookmark-id)
                    (cons "channel_id" channel-id))
              token: token)
  #t)

(def (bookmarks-list channel-id token: (token #f))
  "List bookmarks in a channel. Returns list of bookmark structs."
  (let* ((resp (slack-api "bookmarks.list"
                          (list (cons "channel_id" channel-id))
                          token: token))
         (bookmarks (hash-get resp 'bookmarks)))
    (if bookmarks (map json->bookmark bookmarks) [])))
