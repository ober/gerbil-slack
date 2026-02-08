;; -*- Gerbil -*-
;; Persistent settings via QSettings.
;; Saves/restores window geometry, splitter sizes, last channel, and preferences.

(import
  :std/sugar
  :gerbil-qt/qt)

(export #t)

;;;; Settings Object

(def *settings* #f)

(def (settings-init!)
  "Initialize QSettings for the application."
  (set! *settings* (qt-settings-create "GerbilSlack" "gerbil-slack")))

(def (settings-close!)
  "Sync and destroy settings."
  (when *settings*
    (qt-settings-sync! *settings*)
    (qt-settings-destroy! *settings*)
    (set! *settings* #f)))

;;;; Window Geometry

(def (settings-save-window! win)
  "Save window size and position."
  (when *settings*
    (qt-settings-begin-group! *settings* "Window")
    (qt-settings-set-int! *settings* "width" (qt-widget-width win))
    (qt-settings-set-int! *settings* "height" (qt-widget-height win))
    (qt-settings-end-group! *settings*)
    (qt-settings-sync! *settings*)))

(def (settings-restore-window! win)
  "Restore window size."
  (when *settings*
    (qt-settings-begin-group! *settings* "Window")
    (let ((w (qt-settings-int *settings* "width" default: 1200))
          (h (qt-settings-int *settings* "height" default: 800)))
      (qt-settings-end-group! *settings*)
      (qt-widget-resize! win w h))))

;;;; Splitter Sizes

(def (settings-save-splitter! key sizes)
  "Save splitter sizes as comma-separated string."
  (when *settings*
    (qt-settings-set-string! *settings*
      (string-append "Splitter/" key)
      (string-join (map number->string sizes) ","))))

(def (settings-restore-splitter key defaults)
  "Restore splitter sizes. Returns list of ints or defaults."
  (if *settings*
    (let ((s (qt-settings-string *settings*
               (string-append "Splitter/" key)
               default: "")))
      (if (string=? s "")
        defaults
        (let ((parts (string-split s #\,)))
          (map (lambda (p) (or (string->number p) 0)) parts))))
    defaults))

;;;; Last Channel

(def (settings-save-channel! channel-id)
  "Save the last selected channel."
  (when (and *settings* channel-id)
    (qt-settings-set-string! *settings* "Channel/lastId" channel-id)
    (qt-settings-sync! *settings*)))

(def (settings-restore-channel)
  "Restore the last selected channel ID. Returns #f if none."
  (if *settings*
    (let ((ch (qt-settings-string *settings* "Channel/lastId" default: "")))
      (if (string=? ch "") #f ch))
    #f))

;;;; Preference Booleans

(def (settings-get-bool key (default #f))
  "Get a boolean preference."
  (if *settings*
    (qt-settings-bool *settings* key default: default)
    default))

(def (settings-set-bool! key value)
  "Set a boolean preference."
  (when *settings*
    (qt-settings-set-bool! *settings* key value)
    (qt-settings-sync! *settings*)))

;; Convenience accessors for common preferences
(def (pref-notifications?) (settings-get-bool "Prefs/notifications" #t))
(def (pref-notifications-set! v) (settings-set-bool! "Prefs/notifications" v))
(def (pref-sounds?) (settings-get-bool "Prefs/sounds" #t))
(def (pref-sounds-set! v) (settings-set-bool! "Prefs/sounds" v))
(def (pref-compact-mode?) (settings-get-bool "Prefs/compactMode" #f))
(def (pref-compact-mode-set! v) (settings-set-bool! "Prefs/compactMode" v))
(def (pref-show-previews?) (settings-get-bool "Prefs/showPreviews" #t))
(def (pref-show-previews-set! v) (settings-set-bool! "Prefs/showPreviews" v))

;;;; Helpers

(def (string-join lst sep)
  "Join a list of strings with separator."
  (if (null? lst) ""
    (let loop ((rest (cdr lst)) (acc (car lst)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

(def (string-split s ch)
  "Split a string by character."
  (let loop ((i 0) (start 0) (acc []))
    (cond
      ((>= i (string-length s))
       (reverse (cons (substring s start i) acc)))
      ((char=? (string-ref s i) ch)
       (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
      (else
       (loop (+ i 1) start acc)))))
