;; -*- Gerbil -*-
;; GUI application lifecycle and main window layout

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/config
  :ober/slack/gui/theme)

(export #t)

;;;; Application State — Widget References
;; These are set during (build-main-window!) and available to other modules.

(def *app* #f)
(def *main-window* #f)
(def *sidebar-search* #f)
(def *channel-list* #f)
(def *dm-list* #f)
(def *channel-name-label* #f)
(def *channel-topic-label* #f)
(def *message-browser* #f)
(def *message-input* #f)
(def *send-button* #f)

;; Callback hooks — set by other modules to avoid circular imports
(def *on-search-requested* (void))     ; (lambda () ...)
(def *on-preferences-requested* (void)) ; (lambda () ...)
(def *on-channel-info-requested* (void)) ; (lambda () ...)
(def *on-upload-requested* (void))     ; (lambda () ...)

;;;; Main Entry Point

(def (main . args)
  (with-qt-app app
    (set! *app* app)
    (apply-theme! app)
    (build-main-window!)
    (qt-widget-show! *main-window*)
    (qt-app-exec! app)))

;;;; Main Window Builder

(def (build-main-window!)
  "Build the complete main window with sidebar, message area, and input."
  (let* ((win (qt-main-window-create))
         (central (qt-widget-create))
         (main-layout (qt-vbox-layout-create central))
         (splitter (qt-splitter-create QT_HORIZONTAL)))

    (set! *main-window* win)
    (qt-main-window-set-title! win "Gerbil Slack")
    (qt-widget-set-minimum-size! win 900 600)
    (qt-widget-resize! win 1200 800)

    ;; Build panels
    (let ((sidebar (build-sidebar!))
          (content (build-content-area!)))
      (qt-splitter-add-widget! splitter sidebar)
      (qt-splitter-add-widget! splitter content)
      (qt-splitter-set-sizes! splitter '(250 950))
      (qt-splitter-set-collapsible! splitter 0 #f))

    ;; Add splitter to main layout
    (qt-layout-add-widget! main-layout splitter)
    (qt-layout-set-margins! main-layout 0 0 0 0)
    (qt-layout-set-spacing! main-layout 0)
    (qt-main-window-set-central-widget! win central)

    ;; Build menu bar
    (build-menu-bar! win)

    ;; Status bar
    (qt-main-window-set-status-bar-text! win "Not connected")

    win))

;;;; Sidebar

(def (build-sidebar!)
  "Build the sidebar with team header, search, channels, and DMs."
  (let* ((sidebar (qt-widget-create))
         (layout (qt-vbox-layout-create sidebar))

         ;; Team header
         (team-label (qt-label-create "Slack Workspace"))

         ;; Search
         (search (qt-line-edit-create))

         ;; Channels section
         (channels-label (qt-label-create "CHANNELS"))
         (channel-list (qt-list-widget-create))

         ;; DMs section
         (dm-label (qt-label-create "DIRECT MESSAGES"))
         (dm-list (qt-list-widget-create)))

    ;; Store references
    (set! *sidebar-search* search)
    (set! *channel-list* channel-list)
    (set! *dm-list* dm-list)

    ;; Apply styles
    (style-sidebar! sidebar)
    (style-sidebar-header! team-label)
    (style-sidebar-search! search)
    (style-sidebar-label! channels-label)
    (style-sidebar-list! channel-list)
    (style-sidebar-label! dm-label)
    (style-sidebar-list! dm-list)

    ;; Configure search
    (qt-line-edit-set-placeholder! search "Search...")

    ;; Configure lists
    (qt-widget-set-minimum-height! channel-list 100)
    (qt-widget-set-minimum-height! dm-list 80)

    ;; Assemble layout
    (qt-layout-add-widget! layout team-label)
    (qt-layout-add-widget! layout search)
    (qt-layout-add-widget! layout channels-label)
    (qt-layout-add-widget! layout channel-list)
    (qt-layout-add-widget! layout dm-label)
    (qt-layout-add-widget! layout dm-list)
    (qt-layout-add-stretch! layout)
    (qt-layout-set-margins! layout 0 0 0 0)
    (qt-layout-set-spacing! layout 0)

    ;; Set minimum width
    (qt-widget-set-minimum-width! sidebar 200)

    sidebar))

;;;; Content Area

(def (build-content-area!)
  "Build the main content area: channel header + messages + input."
  (let* ((content (qt-widget-create))
         (layout (qt-vbox-layout-create content)))

    ;; Channel header
    (let ((header (build-channel-header!)))
      (qt-layout-add-widget! layout header))

    ;; Message display
    (let ((browser (qt-text-browser-create)))
      (set! *message-browser* browser)
      (style-message-area! browser)
      (qt-text-browser-set-open-external-links! browser #t)
      (qt-text-browser-set-html! browser
        "<p style='color: #616061; text-align: center; margin-top: 40px;'>Select a channel to start messaging</p>")
      (qt-layout-add-widget! layout browser))

    ;; Input area
    (let ((input-area (build-input-area!)))
      (qt-layout-add-widget! layout input-area))

    ;; Give message browser stretch priority
    (qt-layout-set-stretch-factor! layout 1 1)

    (qt-layout-set-margins! layout 0 0 0 0)
    (qt-layout-set-spacing! layout 0)

    content))

(def (build-channel-header!)
  "Build the channel header bar with name and topic."
  (let* ((header (qt-widget-create))
         (layout (qt-vbox-layout-create header))
         (name-label (qt-label-create "#general"))
         (topic-label (qt-label-create "")))

    (set! *channel-name-label* name-label)
    (set! *channel-topic-label* topic-label)

    (style-channel-header! header)
    (style-channel-name! name-label)
    (style-channel-topic! topic-label)

    (qt-layout-add-widget! layout name-label)
    (qt-layout-add-widget! layout topic-label)
    (qt-layout-set-margins! layout 0 0 0 0)
    (qt-layout-set-spacing! layout 0)

    (qt-widget-set-maximum-height! header 60)

    header))

(def (build-input-area!)
  "Build the message input area with text box and send button."
  (let* ((area (qt-widget-create))
         (layout (qt-hbox-layout-create area))
         (input (qt-plain-text-edit-create))
         (send-btn (qt-push-button-create "Send")))

    (set! *message-input* input)
    (set! *send-button* send-btn)

    (style-input-area! area)
    (style-message-input! input)
    (style-send-button! send-btn)

    (qt-plain-text-edit-set-placeholder! input "Message #general")
    (qt-plain-text-edit-set-max-block-count! input 100)
    (qt-widget-set-maximum-height! input 80)

    ;; Wire send button
    (qt-on-clicked! send-btn on-send-clicked!)

    (qt-layout-add-widget! layout input)
    (qt-layout-add-widget! layout send-btn)
    (qt-layout-set-margins! layout 8 8 8 8)
    (qt-layout-set-spacing! layout 8)

    area))

;;;; Menu Bar

(def (build-menu-bar! win)
  "Build the menu bar with File, Channel, Help menus."
  (let ((menubar (qt-main-window-menu-bar win)))

    ;; File menu
    (let ((file-menu (qt-menu-bar-add-menu menubar "File")))
      (let ((search-action (qt-action-create "Search...")))
        (qt-action-set-shortcut! search-action "Ctrl+F")
        (qt-on-triggered! search-action
          (lambda () (when (procedure? *on-search-requested*) (*on-search-requested*))))
        (qt-menu-add-action! file-menu search-action))
      (let ((upload-action (qt-action-create "Upload File...")))
        (qt-on-triggered! upload-action
          (lambda () (when (procedure? *on-upload-requested*) (*on-upload-requested*))))
        (qt-menu-add-action! file-menu upload-action))
      (qt-menu-add-separator! file-menu)
      (let ((prefs-action (qt-action-create "Preferences...")))
        (qt-action-set-shortcut! prefs-action "Ctrl+,")
        (qt-on-triggered! prefs-action
          (lambda () (when (procedure? *on-preferences-requested*) (*on-preferences-requested*))))
        (qt-menu-add-action! file-menu prefs-action))
      (qt-menu-add-separator! file-menu)
      (let ((quit-action (qt-action-create "Quit")))
        (qt-action-set-shortcut! quit-action "Ctrl+Q")
        (qt-on-triggered! quit-action (lambda () (qt-app-quit! *app*)))
        (qt-menu-add-action! file-menu quit-action)))

    ;; Channel menu
    (let ((channel-menu (qt-menu-bar-add-menu menubar "Channel")))
      (let ((info-action (qt-action-create "Channel Info")))
        (qt-on-triggered! info-action
          (lambda () (when (procedure? *on-channel-info-requested*) (*on-channel-info-requested*))))
        (qt-menu-add-action! channel-menu info-action))
      (let ((members-action (qt-action-create "Members")))
        (qt-menu-add-action! channel-menu members-action))
      (let ((topic-action (qt-action-create "Set Topic...")))
        (qt-menu-add-action! channel-menu topic-action)))

    ;; Help menu
    (let ((help-menu (qt-menu-bar-add-menu menubar "Help")))
      (let ((about-action (qt-action-create "About Gerbil Slack")))
        (qt-on-triggered! about-action on-about!)
        (qt-menu-add-action! help-menu about-action)))))

;;;; Event Handlers (stubs for now)

(def (on-send-clicked!)
  "Handle send button click."
  (let ((text (qt-plain-text-edit-text *message-input*)))
    (when (and text (> (string-length text) 0))
      (qt-plain-text-edit-clear! *message-input*)
      ;; TODO: Actually send the message via chat-post-message
      (qt-main-window-set-status-bar-text! *main-window*
        "Message sent (not connected)"))))

(def (on-about!)
  "Show about dialog."
  (qt-message-box-information *main-window*
    "About Gerbil Slack"
    (string-append "Gerbil Slack v" version
                   "\n\nA Slack client built with Gerbil Scheme and Qt."
                   "\n\nKeyboard shortcuts:"
                   "\n  Ctrl+F    Search"
                   "\n  Ctrl+,    Preferences"
                   "\n  Ctrl+Q    Quit")))

;;;; Public API

(def (set-channel! name topic)
  "Update the channel header display."
  (when *channel-name-label*
    (qt-label-set-text! *channel-name-label* name))
  (when *channel-topic-label*
    (qt-label-set-text! *channel-topic-label* (or topic ""))))

(def (set-status! text)
  "Update the status bar text."
  (when *main-window*
    (qt-main-window-set-status-bar-text! *main-window* text)))

(def (set-messages-html! html)
  "Set the messages area content as HTML."
  (when *message-browser*
    (qt-text-browser-set-html! *message-browser* html)))

(def (append-message-html! html)
  "Append HTML to the messages area."
  (when *message-browser*
    (qt-text-browser-append! *message-browser* html)))
