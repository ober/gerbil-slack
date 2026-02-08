;; -*- Gerbil -*-
;; System tray icon with desktop notifications for new messages.

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/gui/app
  :ober/slack/gui/settings)

(export #t)

;;;; State

(def *tray-icon* #f)
(def *tray-qt-icon* #f)
(def *tray-pixmap* #f)

;;;; Public API

(def (tray-init!)
  "Initialize system tray icon. Returns #t if tray is available."
  (when (qt-system-tray-available?)
    ;; Create a simple colored icon
    (set! *tray-pixmap* (qt-pixmap-create-blank 32 32))
    (qt-pixmap-fill! *tray-pixmap* 74 21 75)  ;; Slack aubergine color
    (set! *tray-qt-icon* (qt-icon-create-from-pixmap *tray-pixmap*))
    (set! *tray-icon* (qt-system-tray-icon-create *tray-qt-icon*
                         parent: *main-window*))
    (qt-system-tray-icon-set-tooltip! *tray-icon* "Gerbil Slack")

    ;; Build tray context menu
    (let ((menu (qt-menu-bar-add-menu
                  (qt-main-window-menu-bar *main-window*) "tray-ctx")))
      (let ((show-action (qt-action-create "Show Window" parent: *main-window*)))
        (qt-on-triggered! show-action
          (lambda () (qt-widget-show-normal! *main-window*)))
        (qt-menu-add-action! menu show-action))
      (qt-menu-add-separator! menu)
      (let ((quit-action (qt-action-create "Quit" parent: *main-window*)))
        (qt-on-triggered! quit-action
          (lambda ()
            (qt-system-tray-icon-hide! *tray-icon*)
            (qt-app-quit! *app*)))
        (qt-menu-add-action! menu quit-action))
      (qt-system-tray-icon-set-context-menu! *tray-icon* menu))

    ;; Handle tray activation (click to show window)
    (qt-on-tray-activated! *tray-icon*
      (lambda (reason)
        (when (= reason QT_TRAY_TRIGGER)
          (qt-widget-show-normal! *main-window*))))

    (qt-system-tray-icon-show! *tray-icon*)
    #t))

(def (tray-destroy!)
  "Clean up tray icon resources."
  (when *tray-icon*
    (qt-system-tray-icon-hide! *tray-icon*)
    (qt-system-tray-icon-destroy! *tray-icon*)
    (set! *tray-icon* #f))
  (when *tray-qt-icon*
    (qt-icon-destroy! *tray-qt-icon*)
    (set! *tray-qt-icon* #f))
  (when *tray-pixmap*
    (qt-pixmap-destroy! *tray-pixmap*)
    (set! *tray-pixmap* #f)))

(def (tray-notify! title message)
  "Show a desktop notification balloon if notifications are enabled."
  (when (and *tray-icon* (pref-notifications?))
    (qt-system-tray-icon-show-message! *tray-icon*
      title message
      icon-type: QT_TRAY_INFO timeout: 5000)))

(def (tray-notify-message! username text)
  "Show a notification for a new message."
  (tray-notify! (string-append username " says:")
                (if (> (string-length text) 100)
                  (string-append (substring text 0 97) "...")
                  text)))

(def (tray-notify-mention! username channel text)
  "Show a notification for an @mention."
  (tray-notify! (string-append "Mention in #" channel)
                (string-append username ": "
                  (if (> (string-length text) 80)
                    (string-append (substring text 0 77) "...")
                    text))))
