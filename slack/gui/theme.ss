;; -*- Gerbil -*-
;; Slack-inspired theme: colors, fonts, stylesheet helpers

(import :gerbil-qt/qt)

(export #t)

;;;; Color Palette (Slack-inspired)

;; Sidebar
(def sidebar-bg "#3F0E40")
(def sidebar-text "#FFFFFF")
(def sidebar-text-dim "#CFC3CF")
(def sidebar-selected "#1264A3")
(def sidebar-hover "#350D36")
(def sidebar-border "#522653")

;; Message area
(def message-bg "#FFFFFF")
(def message-text "#1D1C1D")
(def message-text-dim "#616061")
(def message-link "#1264A3")
(def message-border "#DDDDDD")

;; Input area
(def input-bg "#FFFFFF")
(def input-border "#DDDDDD")
(def input-focus-border "#1264A3")

;; Accents
(def accent-blue "#1264A3")
(def accent-green "#007A5A")
(def accent-red "#E01E5A")
(def accent-yellow "#ECB22E")

;; Status bar
(def status-bg "#F8F8F8")
(def status-border "#E8E8E8")
(def status-text "#616061")

;;;; Widget Style Helpers

(def (style-sidebar! widget)
  "Apply sidebar styling to a container widget."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " sidebar-bg ";")))

(def (style-sidebar-list! widget)
  "Apply sidebar list styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QListWidget {"
     "  background-color: transparent;"
     "  border: none;"
     "  outline: none;"
     "  color: " sidebar-text-dim ";"
     "}"
     "QListWidget::item {"
     "  padding: 4px 16px;"
     "  border: none;"
     "}"
     "QListWidget::item:selected {"
     "  background-color: " sidebar-selected ";"
     "  color: " sidebar-text ";"
     "}"
     "QListWidget::item:hover {"
     "  background-color: " sidebar-hover ";"
     "}")))

(def (style-sidebar-search! widget)
  "Apply sidebar search input styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " sidebar-hover ";"
     "border: 1px solid " sidebar-border ";"
     "border-radius: 4px;"
     "padding: 4px 8px;"
     "color: " sidebar-text ";"
     "font-size: 12px;")))

(def (style-sidebar-label! widget)
  "Apply sidebar section label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " sidebar-text-dim ";"
     "background: transparent;"
     "font-size: 11px;"
     "font-weight: bold;"
     "padding: 8px 16px 4px 16px;")))

(def (style-sidebar-header! widget)
  "Apply sidebar team header styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " sidebar-text ";"
     "background: transparent;"
     "font-size: 15px;"
     "font-weight: bold;"
     "padding: 12px 16px;")))

(def (style-channel-header! widget)
  "Apply channel header bar styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " message-bg ";"
     "border-bottom: 1px solid " message-border ";")))

(def (style-channel-name! widget)
  "Apply channel name label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " message-text ";"
     "background: transparent;"
     "font-size: 16px;"
     "font-weight: bold;"
     "padding: 8px 16px;")))

(def (style-channel-topic! widget)
  "Apply channel topic label styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "color: " message-text-dim ";"
     "background: transparent;"
     "font-size: 12px;"
     "padding: 0 16px 8px 16px;")))

(def (style-message-area! widget)
  "Apply message display area styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QTextBrowser {"
     "  background-color: " message-bg ";"
     "  border: none;"
     "  color: " message-text ";"
     "  font-size: 14px;"
     "}")))

(def (style-input-area! widget)
  "Apply input container styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "background-color: " message-bg ";"
     "border-top: 1px solid " message-border ";")))

(def (style-message-input! widget)
  "Apply message input box styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QPlainTextEdit {"
     "  background-color: " input-bg ";"
     "  border: 1px solid " input-border ";"
     "  border-radius: 4px;"
     "  padding: 8px;"
     "  color: " message-text ";"
     "  font-size: 14px;"
     "}"
     "QPlainTextEdit:focus {"
     "  border-color: " input-focus-border ";"
     "}")))

(def (style-send-button! widget)
  "Apply send button styling."
  (qt-widget-set-style-sheet! widget
    (string-append
     "QPushButton {"
     "  background-color: " accent-green ";"
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
   "  background-color: " sidebar-bg ";"
   "  color: " sidebar-text ";"
   "  border-bottom: 1px solid " sidebar-border ";"
   "}"
   "QMenuBar::item:selected { background-color: " sidebar-hover "; }"
   "QMenu {"
   "  background-color: " message-bg ";"
   "  color: " message-text ";"
   "  border: 1px solid " message-border ";"
   "}"
   "QMenu::item:selected {"
   "  background-color: " accent-blue ";"
   "  color: white;"
   "}"

   ;; Status bar
   "QStatusBar {"
   "  background-color: " status-bg ";"
   "  border-top: 1px solid " status-border ";"
   "  color: " status-text ";"
   "  font-size: 11px;"
   "}"

   ;; Splitter
   "QSplitter::handle {"
   "  background-color: " message-border ";"
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
