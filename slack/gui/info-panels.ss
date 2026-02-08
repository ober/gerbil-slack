;; -*- Gerbil -*-
;; User info and channel info dialog panels

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/cache
  :ober/slack/api/users
  :ober/slack/api/conversations
  :ober/slack/gui/app
  :ober/slack/gui/theme
  :ober/slack/gui/channel-view)

(export #t)

;;;; User Info Dialog

(def (show-user-info! user-id)
  "Show a dialog with user details."
  (let ((u (or (cache-get-user user-id)
               (try (users-info user-id) (catch (e) #f)))))
    (when u
      (let* ((dlg (qt-dialog-create parent: *main-window*))
             (layout (qt-vbox-layout-create dlg))
             (name-label (qt-label-create (or (user-display u) user-id)))
             (details (qt-text-browser-create)))

        (qt-dialog-set-title! dlg "User Info")
        (qt-widget-set-minimum-size! dlg 350 300)

        ;; Style name label
        (qt-widget-set-style-sheet! name-label
          (string-append "font-weight:bold; font-size:18px; color:" clr-message-text
                         "; padding:12px; background:transparent;"))

        ;; Build info HTML
        (let ((html (string-append
                      "<div style='font-family:-apple-system,sans-serif; padding:8px;'>"
                      (info-row "Display Name" (or (user-display-name u) "—"))
                      (info-row "Real Name" (or (user-real-name u) "—"))
                      (info-row "Username" (or (user-name u) "—"))
                      (info-row "Email" (or (user-email u) "—"))
                      (info-row "Timezone" (or (user-tz u) "—"))
                      (info-row "Status" (let ((txt (user-status-text u))
                                               (em (user-status-emoji u)))
                                           (string-append (or em "") " " (or txt "—"))))
                      (info-row "Presence" (if (user-presence u)
                                             (symbol->string (user-presence u))
                                             "—"))
                      (if (user-is-bot u) (info-row "Type" "Bot") "")
                      (if (user-is-admin u) (info-row "Role" "Admin") "")
                      "</div>")))
          (qt-text-browser-set-html! details html))

        ;; DM button
        (let ((dm-btn (qt-push-button-create "Message")))
          (style-send-button! dm-btn)
          (qt-on-clicked! dm-btn
            (lambda ()
              (qt-dialog-accept! dlg)
              ;; Open DM with this user
              (try
                (let ((ch-id (conversations-open users: user-id)))
                  (channel-view-load! ch-id))
                (catch (e) (set-status! "Failed to open DM")))))

          ;; Assemble
          (qt-layout-add-widget! layout name-label)
          (qt-layout-add-widget! layout details)
          (qt-layout-add-widget! layout dm-btn)
          (qt-layout-set-margins! layout 0 0 0 8))

        (qt-dialog-exec! dlg)))))

;;;; Channel Info Dialog

(def (show-channel-info! channel-id)
  "Show a dialog with channel details."
  (let ((ch (or (cache-get-channel channel-id)
                (try (conversations-info channel-id) (catch (e) #f)))))
    (when ch
      (let* ((dlg (qt-dialog-create parent: *main-window*))
             (layout (qt-vbox-layout-create dlg))
             (name-label (qt-label-create
                           (string-append "# " (or (channel-name ch) channel-id))))
             (details (qt-text-browser-create)))

        (qt-dialog-set-title! dlg "Channel Info")
        (qt-widget-set-minimum-size! dlg 400 350)

        ;; Style name label
        (qt-widget-set-style-sheet! name-label
          (string-append "font-weight:bold; font-size:18px; color:" clr-message-text
                         "; padding:12px; background:transparent;"))

        ;; Build info HTML
        (let ((html (string-append
                      "<div style='font-family:-apple-system,sans-serif; padding:8px;'>"
                      (info-row "Name" (or (channel-name ch) "—"))
                      (info-row "Topic" (or (channel-topic ch) "—"))
                      (info-row "Purpose" (or (channel-purpose ch) "—"))
                      (info-row "Members" (if (channel-num-members ch)
                                            (number->string (channel-num-members ch))
                                            "—"))
                      (info-row "Type" (cond
                                         ((channel-is-im ch) "Direct Message")
                                         ((channel-is-mpim ch) "Group DM")
                                         ((channel-is-private ch) "Private Channel")
                                         (else "Public Channel")))
                      (if (channel-is-archived ch) (info-row "Status" "Archived") "")
                      (info-row "Creator" (or (channel-creator ch) "—"))
                      "</div>")))
          (qt-text-browser-set-html! details html))

        ;; Assemble
        (qt-layout-add-widget! layout name-label)
        (qt-layout-add-widget! layout details)
        (qt-layout-set-margins! layout 0 0 0 8)

        (qt-dialog-exec! dlg)))))

;;;; Helper

(def (info-row label value)
  "Generate an HTML row for an info panel."
  (string-append
    "<div style='padding:4px 0; border-bottom:1px solid #F0F0F0;'>"
    "<span style='color:" clr-message-text-dim "; font-size:12px;'>"
    (html-escape label) "</span><br/>"
    "<span style='color:" clr-message-text "; font-size:14px;'>"
    (html-escape value) "</span></div>"))
