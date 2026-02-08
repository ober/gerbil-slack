;; -*- Gerbil -*-
;; Preferences dialog with persistent settings

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/config
  :ober/slack/cache
  :ober/slack/gui/app
  :ober/slack/gui/theme
  :ober/slack/gui/settings)

(export #t)

;;;; Preferences Dialog

(def (show-preferences!)
  "Show the preferences dialog with real controls."
  (let* ((dlg (qt-dialog-create parent: *main-window*))
         (layout (qt-vbox-layout-create dlg))

         ;; Notifications group
         (notif-group (qt-group-box-create "Notifications"))
         (notif-layout (qt-vbox-layout-create notif-group))
         (notif-check (qt-check-box-create "Enable desktop notifications"))
         (sound-check (qt-check-box-create "Enable notification sounds"))
         (preview-check (qt-check-box-create "Show message previews in notifications"))

         ;; Appearance group
         (appear-group (qt-group-box-create "Appearance"))
         (appear-layout (qt-vbox-layout-create appear-group))
         (compact-check (qt-check-box-create "Compact mode (less padding)"))

         ;; Account section
         (account-group (qt-group-box-create "Account"))
         (account-layout (qt-vbox-layout-create account-group))
         (token-info (qt-label-create "Token: configured via ~/.slack.json or SLACK_TOKEN env"))

         ;; Cache section
         (cache-group (qt-group-box-create "Cache"))
         (cache-layout (qt-vbox-layout-create cache-group))
         (clear-btn (qt-push-button-create "Clear Cache"))

         ;; Version info
         (version-label (qt-label-create (string-append "Gerbil Slack v" version)))

         ;; Buttons
         (button-area (qt-widget-create))
         (button-layout (qt-hbox-layout-create button-area))
         (save-btn (qt-push-button-create "Save"))
         (close-btn (qt-push-button-create "Cancel")))

    (qt-dialog-set-title! dlg "Preferences")
    (qt-widget-set-minimum-size! dlg 450 400)

    ;; Style group boxes
    (for-each (lambda (gb)
                (qt-widget-set-style-sheet! gb
                  (string-append
                    "QGroupBox { font-weight:bold; color:" clr-message-text
                    "; border:1px solid " clr-message-border
                    "; border-radius:4px; margin-top:12px; padding-top:16px; }"
                    "QGroupBox::title { subcontrol-origin:margin; left:8px; padding:0 4px; }")))
              (list notif-group appear-group account-group cache-group))

    ;; Style info labels
    (qt-widget-set-style-sheet! token-info
      (string-append "color:" clr-message-text-dim "; font-size:12px;"))
    (qt-widget-set-style-sheet! version-label
      (string-append "color:" clr-message-text-dim "; font-size:12px; padding:4px;"))

    ;; Load current preference values
    (qt-check-box-set-checked! notif-check (pref-notifications?))
    (qt-check-box-set-checked! sound-check (pref-sounds?))
    (qt-check-box-set-checked! preview-check (pref-show-previews?))
    (qt-check-box-set-checked! compact-check (pref-compact-mode?))

    ;; Tooltips
    (qt-widget-set-tooltip! notif-check "Show desktop notifications for new messages")
    (qt-widget-set-tooltip! sound-check "Play a sound when new messages arrive")
    (qt-widget-set-tooltip! preview-check "Include message text in notification balloons")
    (qt-widget-set-tooltip! compact-check "Reduce spacing between messages")
    (qt-widget-set-tooltip! clear-btn "Clear the local message cache")

    ;; Wire clear cache
    (qt-on-clicked! clear-btn
      (lambda ()
        (try
          (cache-close!)
          (cache-open!)
          (set-status! "Cache cleared")
          (catch (e) (set-status! "Failed to clear cache")))))

    ;; Wire save
    (qt-on-clicked! save-btn
      (lambda ()
        (pref-notifications-set! (qt-check-box-checked? notif-check))
        (pref-sounds-set! (qt-check-box-checked? sound-check))
        (pref-show-previews-set! (qt-check-box-checked? preview-check))
        (pref-compact-mode-set! (qt-check-box-checked? compact-check))
        (set-status! "Preferences saved")
        (qt-dialog-accept! dlg)))

    ;; Wire close
    (qt-on-clicked! close-btn (lambda () (qt-dialog-accept! dlg)))
    (style-send-button! save-btn)

    ;; Assemble notification group
    (qt-layout-add-widget! notif-layout notif-check)
    (qt-layout-add-widget! notif-layout sound-check)
    (qt-layout-add-widget! notif-layout preview-check)

    ;; Assemble appearance group
    (qt-layout-add-widget! appear-layout compact-check)

    ;; Assemble account group
    (qt-layout-add-widget! account-layout token-info)

    ;; Assemble cache group
    (qt-layout-add-widget! cache-layout clear-btn)

    ;; Assemble buttons
    (qt-layout-add-stretch! button-layout)
    (qt-layout-add-widget! button-layout save-btn)
    (qt-layout-add-widget! button-layout close-btn)
    (qt-layout-set-margins! button-layout 0 8 0 0)

    ;; Main layout
    (qt-layout-add-widget! layout notif-group)
    (qt-layout-add-widget! layout appear-group)
    (qt-layout-add-widget! layout account-group)
    (qt-layout-add-widget! layout cache-group)
    (qt-layout-add-stretch! layout)
    (qt-layout-add-widget! layout version-label)
    (qt-layout-add-widget! layout button-area)
    (qt-layout-set-margins! layout 12 12 12 12)
    (qt-layout-set-spacing! layout 4)

    (qt-dialog-exec! dlg)))
