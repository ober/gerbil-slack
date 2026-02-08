;; -*- Gerbil -*-
;; Slack-inspired theme: colors, fonts, stylesheet helpers
;; Color constants are prefixed with clr- to avoid import conflicts with types module.

(import :gerbil-qt/qt)

(export #t)

;;;; Color Palette (Slack-inspired)

;; Sidebar
(def clr-sidebar-bg "#3F0E40")
(def clr-sidebar-text "#FFFFFF")
(def clr-sidebar-text-dim "#CFC3CF")
(def clr-sidebar-selected "#1264A3")
(def clr-sidebar-hover "#350D36")
(def clr-sidebar-border "#522653")

;; Message area
(def clr-message-bg "#FFFFFF")
(def clr-message-text "#1D1C1D")
(def clr-message-text-dim "#616061")
(def clr-message-link "#1264A3")
(def clr-message-border "#DDDDDD")

;; Input area
(def clr-input-bg "#FFFFFF")
(def clr-input-border "#DDDDDD")
(def clr-input-focus-border "#1264A3")

;; Accents
(def clr-accent-blue "#1264A3")
(def clr-accent-green "#007A5A")
(def clr-accent-red "#E01E5A")
(def clr-accent-yellow "#ECB22E")

;; Status bar
(def clr-status-bg "#F8F8F8")
(def clr-status-border "#E8E8E8")
(def clr-status-text "#616061")

;;;; Widget Style Helpers

(def (style-sidebar! widget)
  "Apply sidebar styling to a container widget."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " clr-sidebar-bg ";")))

(def (style-sidebar-list! widget)
  "Apply sidebar list styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QListWidget {"
     "  background-color: transparent;"
     "  border: none;"
     "  outline: none;"
     "  color: " clr-sidebar-text-dim ";"
     "}"
     "QListWidget::item {"
     "  padding: 4px 16px;"
     "  border: none;"
     "}"
     "QListWidget::item:selected {"
     "  background-color: " clr-sidebar-selected ";"
     "  color: " clr-sidebar-text ";"
     "}"
     "QListWidget::item:hover {"
     "  background-color: " clr-sidebar-hover ";"
     "}")))

(def (style-sidebar-search! widget)
  "Apply sidebar search input styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " clr-sidebar-hover ";"
     "border: 1px solid " clr-sidebar-border ";"
     "border-radius: 4px;"
     "padding: 4px 8px;"
     "color: " clr-sidebar-text ";"
     "font-size: 12px;")))

(def (style-sidebar-label! widget)
  "Apply sidebar section label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " clr-sidebar-text-dim ";"
     "background: transparent;"
     "font-size: 11px;"
     "font-weight: bold;"
     "padding: 8px 16px 4px 16px;")))

(def (style-sidebar-header! widget)
  "Apply sidebar team header styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " clr-sidebar-text ";"
     "background: transparent;"
     "font-size: 15px;"
     "font-weight: bold;"
     "padding: 12px 16px;")))

(def (style-channel-header! widget)
  "Apply channel header bar styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " clr-message-bg ";"
     "border-bottom: 1px solid " clr-message-border ";")))

(def (style-channel-name! widget)
  "Apply channel name label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " clr-message-text ";"
     "background: transparent;"
     "font-size: 16px;"
     "font-weight: bold;"
     "padding: 8px 16px;")))

(def (style-channel-topic! widget)
  "Apply channel topic label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " clr-message-text-dim ";"
     "background: transparent;"
     "font-size: 12px;"
     "padding: 0 16px 8px 16px;")))

(def (style-message-area! widget)
  "Apply message display area styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QTextBrowser {"
     "  background-color: " clr-message-bg ";"
     "  border: none;"
     "  color: " clr-message-text ";"
     "  font-size: 14px;"
     "}")))

(def (style-input-area! widget)
  "Apply input container styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " clr-message-bg ";"
     "border-top: 1px solid " clr-message-border ";")))

(def (style-message-input! widget)
  "Apply message input box styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QPlainTextEdit {"
     "  background-color: " clr-input-bg ";"
     "  border: 1px solid " clr-input-border ";"
     "  border-radius: 4px;"
     "  padding: 8px;"
     "  color: " clr-message-text ";"
     "  font-size: 14px;"
     "}"
     "QPlainTextEdit:focus {"
     "  border-color: " clr-input-focus-border ";"
     "}")))

(def (style-send-button! widget)
  "Apply send button styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QPushButton {"
     "  background-color: " clr-accent-green ";"
     "  color: white;"
     "  border: none;"
     "  border-radius: 4px;"
     "  padding: 8px 16px;"
     "  font-weight: bold;"
     "}"
     "QPushButton:hover { background-color: #148060; }"
     "QPushButton:pressed { background-color: #005A3A; }")))

;;;; Global App Stylesheet (menus, scrollbars, splitter, status bar)

(def app-stylesheet
  (string-append
   ;; Menu bar
   "QMenuBar {"
   "  background-color: " clr-sidebar-bg ";"
   "  color: " clr-sidebar-text ";"
   "  border-bottom: 1px solid " clr-sidebar-border ";"
   "}"
   "QMenuBar::item:selected { background-color: " clr-sidebar-hover "; }"
   "QMenu {"
   "  background-color: " clr-message-bg ";"
   "  color: " clr-message-text ";"
   "  border: 1px solid " clr-message-border ";"
   "}"
   "QMenu::item:selected {"
   "  background-color: " clr-accent-blue ";"
   "  color: white;"
   "}"

   ;; Status bar
   "QStatusBar {"
   "  background-color: " clr-status-bg ";"
   "  border-top: 1px solid " clr-status-border ";"
   "  color: " clr-status-text ";"
   "  font-size: 11px;"
   "}"

   ;; Splitter
   "QSplitter::handle {"
   "  background-color: " clr-message-border ";"
   "  width: 1px;"
   "}"

   ;; Scrollbars
   "QScrollBar:vertical {"
   "  background-color: transparent;"
   "  width: 8px;"
   "}"
   "QScrollBar::handle:vertical {"
   "  background-color: #C1C1C1;"
   "  border-radius: 4px;"
   "  min-height: 20px;"
   "}"
   "QScrollBar::handle:vertical:hover { background-color: #A1A1A1; }"
   "QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical { height: 0px; }"))

(def (apply-theme! app)
  "Apply the global theme stylesheet to the application."
  (qt-app-set-style-sheet! app app-stylesheet))
