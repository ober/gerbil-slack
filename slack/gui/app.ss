;; -*- Gerbil -*-
;; GUI application lifecycle and main window layout

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/config
  :ober/slack/gui/theme
  :ober/slack/gui/settings)

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
(def *main-splitter* #f)
(def *progress-bar* #f)

;; Callback hooks — set by other modules to avoid circular imports
(def *on-search-requested* (void))     ; (lambda () ...)
(def *on-preferences-requested* (void)) ; (lambda () ...)
(def *on-channel-info-requested* (void)) ; (lambda () ...)
(def *on-upload-requested* (void))     ; (lambda () ...)
(def *on-copy-requested* (void))       ; (lambda () ...)

;;;; Main Entry Point

(def (main . args)
  (with-qt-app app
    (set! *app* app)
    (apply-theme! app)
    (settings-init!)
    (build-main-window!)
    (settings-restore-window! *main-window*)
    (qt-widget-show! *main-window*)
    (qt-app-exec! app)
    ;; Cleanup on exit
    (settings-save-window! *main-window*)
    (settings-close!)))

;;;; Main Window Builder

(def (build-main-window!)
  "Build the complete main window with sidebar, message area, and input."
  (let* ((win (qt-main-window-create))
         (central (qt-widget-create))
         (main-layout (qt-vbox-layout-create central))
         (splitter (qt-splitter-create QT_HORIZONTAL)))

    (set! *main-window* win)
    (set! *main-splitter* splitter)
    (qt-main-window-set-title! win "Gerbil Slack")
    (qt-widget-set-minimum-size! win 900 600)
    (qt-widget-resize! win 1200 800)

    ;; Build panels
    (let ((sidebar (build-sidebar!))
          (content (build-content-area!)))
      (qt-splitter-add-widget! splitter sidebar)
      (qt-splitter-add-widget! splitter content)
      ;; Restore splitter sizes or use defaults
      (let ((sizes (settings-restore-splitter "main" '(250 950))))
        (qt-splitter-set-sizes! splitter sizes))
      (qt-splitter-set-collapsible! splitter 0 #f))

    ;; Add splitter to main layout
    (qt-layout-add-widget! main-layout splitter)
    (qt-layout-set-margins! main-layout 0 0 0 0)
    (qt-layout-set-spacing! main-layout 0)
    (qt-main-window-set-central-widget! win central)

    ;; Build toolbar
    (build-toolbar! win)

    ;; Build menu bar
    (build-menu-bar! win)

    ;; Status bar
    (qt-main-window-set-status-bar-text! win "Not connected")

    win))

;;;; Toolbar

(def (build-toolbar! win)
  "Build the main toolbar with quick-access buttons."
  (let ((toolbar (qt-toolbar-create "Main")))
    (qt-toolbar-set-movable! toolbar #f)

    ;; Search button
    (let ((search-action (qt-action-create "Search" parent: win)))
      (qt-action-set-tooltip! search-action "Search messages (Ctrl+F)")
      (qt-on-triggered! search-action
        (lambda () (when (procedure? *on-search-requested*) (*on-search-requested*))))
      (qt-toolbar-add-action! toolbar search-action))

    ;; Upload button
    (let ((upload-action (qt-action-create "Upload" parent: win)))
      (qt-action-set-tooltip! upload-action "Upload a file")
      (qt-on-triggered! upload-action
        (lambda () (when (procedure? *on-upload-requested*) (*on-upload-requested*))))
      (qt-toolbar-add-action! toolbar upload-action))

    (qt-toolbar-add-separator! toolbar)

    ;; Channel info button
    (let ((info-action (qt-action-create "Info" parent: win)))
      (qt-action-set-tooltip! info-action "Channel information")
      (qt-on-triggered! info-action
        (lambda () (when (procedure? *on-channel-info-requested*) (*on-channel-info-requested*))))
      (qt-toolbar-add-action! toolbar info-action))

    (qt-toolbar-add-separator! toolbar)

    ;; Preferences button
    (let ((prefs-action (qt-action-create "Prefs" parent: win)))
      (qt-action-set-tooltip! prefs-action "Preferences (Ctrl+,)")
      (qt-on-triggered! prefs-action
        (lambda () (when (procedure? *on-preferences-requested*) (*on-preferences-requested*))))
      (qt-toolbar-add-action! toolbar prefs-action))

    (qt-main-window-add-toolbar! win toolbar)))

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
    (qt-line-edit-set-placeholder! search "Search channels...")
    (qt-widget-set-tooltip! search "Filter channels and DMs")

    ;; Tooltips
    (qt-widget-set-tooltip! channel-list "Double-click to open channel")
    (qt-widget-set-tooltip! dm-list "Double-click to open conversation")

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
  "Build the main content area: channel header + messages + progress + input."
  (let* ((content (qt-widget-create))
         (layout (qt-vbox-layout-create content)))

    ;; Channel header
    (let ((header (build-channel-header!)))
      (qt-layout-add-widget! layout header))

    ;; Progress bar (hidden by default)
    (let ((pbar (qt-progress-bar-create parent: content)))
      (set! *progress-bar* pbar)
      (qt-progress-bar-set-range! pbar 0 100)
      (qt-widget-set-maximum-height! pbar 4)
      (qt-widget-set-style-sheet! pbar
        (string-append "QProgressBar { border: none; background: transparent; }"
                       "QProgressBar::chunk { background-color: " clr-accent-blue "; }"))
      (qt-widget-hide! pbar)
      (qt-layout-add-widget! layout pbar))

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
    (qt-layout-set-stretch-factor! layout 2 1)

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

    ;; Tooltips on header labels
    (qt-widget-set-tooltip! name-label "Current channel")
    (qt-widget-set-tooltip! topic-label "Channel topic")

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
    (qt-widget-set-tooltip! input "Type a message (Enter to send, Shift+Enter for new line)")
    (qt-widget-set-tooltip! send-btn "Send message")

    ;; Wire send button
    (qt-on-clicked! send-btn on-send-clicked!)

    (qt-layout-add-widget! layout input)
    (qt-layout-add-widget! layout send-btn)
    (qt-layout-set-margins! layout 8 8 8 8)
    (qt-layout-set-spacing! layout 8)

    area))

;;;; Menu Bar

(def (build-menu-bar! win)
  "Build the menu bar with File, Edit, Channel, Help menus."
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
        (qt-on-triggered! quit-action
          (lambda ()
            (when *main-window* (settings-save-window! *main-window*))
            (qt-app-quit! *app*)))
        (qt-menu-add-action! file-menu quit-action)))

    ;; Edit menu (new)
    (let ((edit-menu (qt-menu-bar-add-menu menubar "Edit")))
      (let ((copy-action (qt-action-create "Copy")))
        (qt-action-set-shortcut! copy-action "Ctrl+C")
        (qt-on-triggered! copy-action
          (lambda () (when (procedure? *on-copy-requested*) (*on-copy-requested*))))
        (qt-menu-add-action! edit-menu copy-action)))

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
                   "\n  Ctrl+C    Copy"
                   "\n  Ctrl+,    Preferences"
                   "\n  Ctrl+Q    Quit")))

;;;; Public API

(def (set-channel! name topic)
  "Update the channel header display."
  (when *channel-name-label*
    (qt-label-set-text! *channel-name-label* name))
  (when *channel-topic-label*
    (qt-label-set-text! *channel-topic-label* (or topic ""))
    ;; Set tooltip to show full topic on hover
    (when topic
      (qt-widget-set-tooltip! *channel-topic-label* topic))))

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

(def (save-splitter-state!)
  "Save the main splitter sizes to settings."
  (when *main-splitter*
    (let ((sizes (list (qt-splitter-size-at *main-splitter* 0)
                       (qt-splitter-size-at *main-splitter* 1))))
      (settings-save-splitter! "main" sizes))))
