;; -*- Gerbil -*-
;; Preferences dialog

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/config
  :ober/slack/cache
  :ober/slack/gui/app
  :ober/slack/gui/theme)

(export #t)

;;;; Preferences Dialog

(def (show-preferences!)
  "Show the preferences dialog."
  (let* ((dlg (qt-dialog-create parent: *main-window*))
         (layout (qt-vbox-layout-create dlg))
         ;; Account section
         (account-label (qt-label-create "Account"))
         (token-info (qt-label-create "Token: configured via ~/.slack.json or SLACK_TOKEN env"))
         ;; Cache section
         (cache-label (qt-label-create "Cache"))
         (clear-btn (qt-push-button-create "Clear Cache"))
         ;; Version info
         (version-label (qt-label-create (string-append "Gerbil Slack v" version)))
         ;; Close
         (close-btn (qt-push-button-create "Close")))

    (qt-dialog-set-title! dlg "Preferences")
    (qt-widget-set-minimum-size! dlg 400 300)

    ;; Style headers
    (for-each (lambda (lbl)
                (qt-widget-set-style-sheet! lbl
                  (string-append "font-weight:bold; font-size:14px; color:" clr-message-text
                                 "; padding:12px 8px 4px 8px; background:transparent;")))
              (list account-label cache-label))

    ;; Style info labels
    (for-each (lambda (lbl)
                (qt-widget-set-style-sheet! lbl
                  (string-append "color:" clr-message-text-dim
                                 "; padding:4px 8px; background:transparent; font-size:12px;")))
              (list token-info version-label))

    ;; Wire clear cache
    (qt-on-clicked! clear-btn
      (lambda ()
        (try
          (cache-close!)
          (cache-open!)
          (set-status! "Cache cleared")
          (catch (e) (set-status! "Failed to clear cache")))))

    ;; Wire close
    (qt-on-clicked! close-btn (lambda () (qt-dialog-accept! dlg)))
    (style-send-button! close-btn)

    ;; Assemble
    (qt-layout-add-widget! layout account-label)
    (qt-layout-add-widget! layout token-info)
    (qt-layout-add-widget! layout cache-label)
    (qt-layout-add-widget! layout clear-btn)
    (qt-layout-add-stretch! layout)
    (qt-layout-add-widget! layout version-label)
    (qt-layout-add-widget! layout close-btn)
    (qt-layout-set-margins! layout 8 8 8 8)
    (qt-layout-set-spacing! layout 4)

    (qt-dialog-exec! dlg)))
