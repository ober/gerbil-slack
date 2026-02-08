;; -*- Gerbil -*-
;; Search dialog for messages and files

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/api/search
  :ober/slack/markdown
  :ober/slack/gui/app
  :ober/slack/gui/theme
  :ober/slack/gui/channel-view)

(export #t)

;;;; Search Dialog

(def (show-search-dialog!)
  "Open the search dialog."
  (let* ((dlg (qt-dialog-create parent: *main-window*))
         (layout (qt-vbox-layout-create dlg))
         ;; Search input
         (search-input (qt-line-edit-create))
         ;; Results browser
         (results (qt-text-browser-create))
         ;; Search button
         (search-btn (qt-push-button-create "Search")))

    (qt-dialog-set-title! dlg "Search")
    (qt-widget-set-minimum-size! dlg 600 500)

    ;; Style
    (qt-line-edit-set-placeholder! search-input "Search messages and files...")
    (qt-widget-set-style-sheet! search-input
      (string-append "padding:8px; font-size:14px; border:1px solid "
                     clr-input-border "; border-radius:4px;"))
    (style-message-area! results)
    (style-send-button! search-btn)

    ;; Wire search
    (let ((do-search
            (lambda ()
              (let ((query (qt-line-edit-text search-input)))
                (when (and query (> (string-length query) 0))
                  (qt-text-browser-set-html! results
                    "<p style='text-align:center; color:#616061;'>Searching...</p>")
                  (let ((result-html (run-search query)))
                    (qt-text-browser-set-html! results result-html)))))))
      (qt-on-clicked! search-btn do-search)
      ;; Also search on Enter
      (qt-on-key-press! search-input
        (lambda ()
          (let ((key (qt-last-key-code)))
            (when (or (= key QT_KEY_RETURN) (= key QT_KEY_ENTER))
              (do-search))))))

    ;; Assemble
    (qt-layout-add-widget! layout search-input)
    (qt-layout-add-widget! layout search-btn)
    (qt-layout-add-widget! layout results)
    (qt-layout-set-margins! layout 8 8 8 8)
    (qt-layout-set-spacing! layout 8)

    (qt-dialog-exec! dlg)))

;;;; Search Execution

(def (run-search query)
  "Execute search and return results as HTML."
  (try
    (let ((msg-results (search-messages query))
          (file-results (search-files query)))
      (string-append
        "<html><body style='margin:0; padding:8px; font-family:-apple-system,sans-serif; "
        "font-size:14px; color:" clr-message-text ";'>"
        ;; Message results
        "<h3 style='color:" clr-message-text-dim ";'>Messages</h3>"
        (if (null? msg-results)
          "<p style='color:#616061;'>No messages found</p>"
          (apply string-append
            (map render-search-message msg-results)))
        ;; File results
        "<h3 style='color:" clr-message-text-dim "; margin-top:16px;'>Files</h3>"
        (if (null? file-results)
          "<p style='color:#616061;'>No files found</p>"
          (apply string-append
            (map render-search-file file-results)))
        "</body></html>"))
    (catch (e)
      (string-append
        "<p style='color:" clr-accent-red ";'>Search failed</p>"))))

(def (render-search-message msg)
  "Render a search result message as HTML."
  (let* ((text (or (message-text msg) ""))
         (user-id (or (message-user msg) ""))
         (ch (or (message-channel msg) ""))
         (ts (or (message-ts msg) "")))
    (string-append
      "<div style='padding:8px; border-bottom:1px solid #F0F0F0;'>"
      "<div style='font-size:11px; color:" clr-message-text-dim ";'>"
      "in " (html-escape ch) " — " (format-timestamp ts) "</div>"
      "<div style='margin-top:4px;'>" (mrkdwn->html text) "</div>"
      "</div>")))

(def (render-search-file f)
  "Render a search result file as HTML."
  (let ((name (or (file-info-name f) "file"))
        (title (or (file-info-title f) "")))
    (string-append
      "<div style='padding:8px; border-bottom:1px solid #F0F0F0;'>"
      "<div>&#128206; <strong>" (html-escape name) "</strong></div>"
      (if (> (string-length title) 0)
        (string-append "<div style='color:" clr-message-text-dim "; font-size:12px;'>"
                       (html-escape title) "</div>")
        "")
      "</div>")))
