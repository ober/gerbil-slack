;; -*- Gerbil -*-
;; Sidebar population: channels, DMs, search/filter, selection signals

(import
  :std/sugar
  :std/iter
  :std/srfi/13
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/config
  :ober/slack/cache
  :ober/slack/api/conversations
  :ober/slack/api/users
  :ober/slack/api/team
  :ober/slack/gui/app
  :ober/slack/gui/theme)

(export #t)

;;;; State

;; Ordered lists matching current widget rows
(def *channel-entries* [])  ;; list of channel structs
(def *dm-entries* [])       ;; list of channel structs (is-im or is-mpim)
(def *active-channel-id* #f)

;; Callback invoked when user selects a channel/DM
;; signature: (lambda (channel-id) ...)
(def *on-channel-selected* #f)

;;;; Public API

(def (sidebar-init! (on-select #f))
  "Initialize sidebar: wire signals, set selection callback."
  (set! *on-channel-selected* on-select)
  ;; Wire channel list selection
  (when *channel-list*
    (qt-on-current-row-changed! *channel-list*
      (lambda (row)
        (when (and (>= row 0) (< row (length *channel-entries*)))
          (let ((ch (list-ref *channel-entries* row)))
            (set! *active-channel-id* (channel-id ch))
            (when *on-channel-selected*
              (*on-channel-selected* (channel-id ch))))))))
  ;; Wire DM list selection
  (when *dm-list*
    (qt-on-current-row-changed! *dm-list*
      (lambda (row)
        (when (and (>= row 0) (< row (length *dm-entries*)))
          (let ((ch (list-ref *dm-entries* row)))
            (set! *active-channel-id* (channel-id ch))
            ;; Deselect channel list when DM selected
            (qt-list-widget-set-current-row! *channel-list* -1)
            (when *on-channel-selected*
              (*on-channel-selected* (channel-id ch))))))))
  ;; Wire search filter
  (when *sidebar-search*
    (qt-on-text-changed! *sidebar-search*
      (lambda (text)
        (sidebar-filter! text)))))

(def (sidebar-refresh!)
  "Reload channels and DMs from cache (or API if stale), and repopulate the lists."
  (let* ((all-channels (get-channels))
         (channels (filter-channels all-channels))
         (dms (filter-dms all-channels)))
    ;; Update team header
    (update-team-header!)
    ;; Populate channel list
    (set! *channel-entries* channels)
    (populate-channel-list! *channel-list* channels)
    ;; Populate DM list
    (set! *dm-entries* dms)
    (populate-dm-list! *dm-list* dms)
    ;; Restore selection if we had one
    (when *active-channel-id*
      (sidebar-set-active! *active-channel-id*))))

(def (sidebar-set-active! channel-id)
  "Highlight the given channel/DM in the sidebar."
  (set! *active-channel-id* channel-id)
  ;; Try channel list first
  (let ((idx (find-entry-index *channel-entries* channel-id)))
    (if idx
      (begin
        (qt-list-widget-set-current-row! *channel-list* idx)
        (qt-list-widget-set-current-row! *dm-list* -1))
      ;; Try DM list
      (let ((dm-idx (find-entry-index *dm-entries* channel-id)))
        (when dm-idx
          (qt-list-widget-set-current-row! *dm-list* dm-idx)
          (qt-list-widget-set-current-row! *channel-list* -1))))))

(def (sidebar-filter! text)
  "Filter sidebar lists by search text."
  (let ((query (string-downcase text)))
    (if (string=? query "")
      ;; Empty filter: show all
      (begin
        (populate-channel-list! *channel-list* *channel-entries*)
        (populate-dm-list! *dm-list* *dm-entries*))
      ;; Filter both lists
      (let ((filtered-ch (filter (lambda (ch)
                                   (let ((name (or (channel-name ch) "")))
                                     (string-contains name query)))
                                 *channel-entries*))
            (filtered-dm (filter (lambda (ch)
                                   (let ((name (or (channel-name ch) "")))
                                     (string-contains name query)))
                                 *dm-entries*)))
        (populate-channel-list! *channel-list* filtered-ch)
        (populate-dm-list! *dm-list* filtered-dm)))))

;;;; Internal Helpers

(def (get-channels)
  "Get channel list from cache, refreshing from API if stale."
  (when (cache-channels-stale?)
    (let ((fresh (conversations-list)))
      (for-each cache-channel! fresh)
      (cache-mark-channels-fresh!)))
  (cache-all-channels))

(def (filter-channels all)
  "Filter to regular channels (not IMs/MPIMs), non-archived, that user is a member of."
  (filter (lambda (ch)
            (and (not (channel-is-im ch))
                 (not (channel-is-mpim ch))
                 (not (channel-is-archived ch))))
          all))

(def (filter-dms all)
  "Filter to DMs and MPIMs, non-archived."
  (filter (lambda (ch)
            (and (or (channel-is-im ch) (channel-is-mpim ch))
                 (not (channel-is-archived ch))))
          all))

(def (channel-display-label ch)
  "Format a channel for display in the sidebar."
  (let ((name (or (channel-name ch) (channel-id ch))))
    (if (channel-is-private ch)
      (string-append "  " name)
      (string-append "  # " name))))

(def (dm-display-label ch)
  "Format a DM for display in the sidebar."
  (let ((name (or (channel-name ch) (channel-id ch))))
    (string-append "  " name)))

(def (populate-channel-list! widget channels)
  "Clear and repopulate a list widget with channel entries."
  (when widget
    (qt-list-widget-clear! widget)
    (for-each
      (lambda (ch)
        (qt-list-widget-add-item! widget (channel-display-label ch)))
      channels)))

(def (populate-dm-list! widget dms)
  "Clear and repopulate a list widget with DM entries."
  (when widget
    (qt-list-widget-clear! widget)
    (for-each
      (lambda (ch)
        (qt-list-widget-add-item! widget (dm-display-label ch)))
      dms)))

(def (find-entry-index entries channel-id)
  "Find the index of a channel-id in a list of channel structs."
  (let loop ((es entries) (i 0))
    (cond
      ((null? es) #f)
      ((equal? (channel-id (car es)) channel-id) i)
      (else (loop (cdr es) (+ i 1))))))

(def (update-team-header!)
  "Update the team header label with workspace name."
  ;; Team header is a label in the sidebar — we find it from config
  (try
    (let ((ti (team-info)))
      (when (and ti *main-window*)
        (qt-main-window-set-title! *main-window*
          (string-append "Gerbil Slack — " (or (team-name ti) "Workspace")))))
    (catch (e)
      ;; If team-info fails (no token, network), silently ignore
      (void))))
