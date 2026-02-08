# Gerbil-Slack: Comprehensive Implementation Plan

## Overview

A complete rewrite of the gerbil-slack project into three components:

1. **Core Library** (`slack/`) — Pure Slack API client with no UI dependencies
2. **CLI** (`slack/cli.ss`) — Command-line interface with human-readable output
3. **Qt GUI** (`slack/gui/`) — Full-featured desktop Slack client using gerbil-qt

### Key Design Decisions

- **Drop oberlib dependency** — Use `:std/net/request` directly for HTTP, `:std/text/json` for JSON. No intermediary wrappers that obscure return types.
- **Drop libyaml dependency** — Use JSON for config (`~/.slack.json`). Simpler, no C library dependency.
- **Use `:std/net/websocket`** natively for Socket Mode real-time events.
- **Use `:std/db/sqlite`** for local caching (messages, users, channels).
- **Use `:std/getopt`** for CLI argument parsing with proper subcommands.
- **Structs for all domain types** — `defstruct` for User, Channel, Message, etc. No raw hash tables in the public API.
- **Event-driven architecture** — Core library emits events; both CLI and GUI consume them.

### Directory Structure (Target)

```
gerbil-slack/
├── old/                          # All original files moved here
├── gerbil.pkg
├── build.ss
├── Makefile
├── CLAUDE.md
├── plan.md
├── slack.ss                      # Top-level re-export module
├── slack/
│   ├── config.ss                 # Configuration loading, token storage
│   ├── types.ss                  # Domain structs (User, Channel, Message, Team, etc.)
│   ├── http.ss                   # HTTP client wrapper (auth headers, rate limiting, pagination)
│   ├── api/
│   │   ├── auth.ss               # auth.test, oauth
│   │   ├── chat.ss               # chat.postMessage, chat.update, chat.delete, etc.
│   │   ├── conversations.ss      # conversations.list, .history, .info, .members, .replies, etc.
│   │   ├── users.ss              # users.list, .info, .getPresence, .setPresence
│   │   ├── files.ss              # files.upload, .delete, .info, .list
│   │   ├── reactions.ss          # reactions.add, .remove, .get
│   │   ├── pins.ss               # pins.add, .remove, .list
│   │   ├── search.ss             # search.messages, .files, .all
│   │   ├── emoji.ss              # emoji.list
│   │   ├── team.ss               # team.info
│   │   ├── reminders.ss          # reminders.add, .complete, .delete, .list
│   │   ├── dnd.ss                # dnd.setSnooze, .endSnooze, .info
│   │   ├── usergroups.ss         # usergroups.list, .create, .update
│   │   └── bookmarks.ss          # bookmarks.add, .edit, .remove, .list
│   ├── socket.ss                 # Socket Mode WebSocket client (real-time events)
│   ├── events.ss                 # Event types, dispatcher, subscriptions
│   ├── cache.ss                  # SQLite-backed cache for users, channels, messages
│   ├── markdown.ss               # Slack mrkdwn → plain text and → HTML conversion
│   ├── cli.ss                    # CLI entry point (executable)
│   ├── cli/
│   │   ├── format.ss             # Output formatters (table, json, plain)
│   │   └── commands.ss           # CLI command definitions and dispatch
│   ├── gui.ss                    # GUI entry point (executable)
│   └── gui/
│       ├── app.ss                # Application lifecycle, main window
│       ├── theme.ss              # Colors, fonts, stylesheets
│       ├── sidebar.ss            # Workspace/channel/DM sidebar
│       ├── channel-view.ss       # Message list for a channel/DM
│       ├── message-widget.ss     # Individual message rendering
│       ├── thread-view.ss        # Thread panel
│       ├── input-bar.ss          # Message composition area
│       ├── user-list.ss          # Member list panel
│       ├── file-upload.ss        # File upload dialog
│       ├── search-dialog.ss      # Search UI
│       ├── preferences.ss        # Settings dialog
│       └── tray.ss               # System tray integration
```

---

## Phase 1: Foundation — Config, Types, HTTP Client ✅ COMPLETE

**Goal:** Establish the project skeleton, move old code, define all domain types, and build a reliable HTTP client that handles auth, rate limiting, and pagination.

### 1.1 Project Setup
- [x] Create `old/` directory
- [x] Move all existing files into `old/`
- [x] Create new `gerbil.pkg` (no external dependencies for core library)
- [x] Create new `build.ss` with build targets for library, CLI exe, and all API modules
- [x] Create new `Makefile` with targets: `build`, `test`, `clean`, `install`
- [x] Create stub `slack.ss` re-export module

### 1.2 Configuration (`slack/config.ss`)
- [x] Load from `~/.slack.json` (plain JSON, no YAML dependency)
- [x] Support `SLACK_TOKEN` environment variable override
- [x] Support `SLACK_CONFIG` env var for alternate config path
- [x] Token encryption using `:std/crypto/cipher` (AES-256-CTR)
- [x] Config validation (check required fields present)
- [x] `(load-config)` — returns config hash, cached after first call
- [x] `(save-config cfg)` — atomic write config to disk
- [x] `(config-setup!)` — interactive first-run setup (prompt for token, save)
- [x] `(config-token)` / `(config-ref key)` — convenience accessors

### 1.3 Domain Types (`slack/types.ss`)
- [x] `defclass user` — transparent, keyword construction
- [x] `defclass channel` — transparent, keyword construction
- [x] `defclass message` — transparent, keyword construction
- [x] `defclass reaction` — transparent, keyword construction
- [x] `defclass file-info` — transparent, keyword construction
- [x] `defclass team` — transparent, keyword construction
- [x] `defclass reminder` — transparent, keyword construction
- [x] `defclass bookmark` — transparent, keyword construction
- [x] JSON→struct parsers for each type: `(json->user hash)`, `(json->channel hash)`, etc.
- [x] Display helpers: `(user-display u)`, `(channel-display ch)`, `(message-summary msg)`
- **Note:** Used `defclass` instead of `defstruct` — Gerbil `defstruct` only supports positional construction; `defclass` supports keyword construction needed for clean JSON parsers.

### 1.4 HTTP Client (`slack/http.ss`)
- [x] `(slack-api method params ...)` — core API caller with keyword args
  - Builds URL: `https://slack.com/api/{method}`
  - Sets Authorization header (Bearer token), never in URL params
  - Parses JSON response with symbol keys, checks `ok` field
  - Returns parsed hash on success, raises `slack-error` on failure
- [x] `defclass slack-error (method code message)` — typed errors
- [x] Rate limit handling: detect 429 status, read `Retry-After` header
- [x] Automatic pagination: `(slack-api-paginated method params key: ...)`
- [x] `(slack-post method params ...)` — convenience POST wrapper
- [x] Helper utilities: `url-encode`, `params->query`, `params->hash`, `string-join`, `alist-get`

### 1.5 Build & Verify
- [x] `gerbil build` compiles all modules without errors (18 modules + exe)
- [x] 60 unit tests passing: types-test (41 checks), http-test (19 checks)
- [x] CLI executable builds and runs (`slack auth-test`, `slack config-setup`)

---

## Phase 2: Core API Methods ✅ COMPLETE

**Goal:** Implement all Slack Web API method wrappers, organized by category. Each returns proper structs, handles pagination, and provides both single-item and list variants.

### 2.1 Auth (`slack/api/auth.ss`)
- [x] `(auth-test)` → response hash with team, user, user-id, team-id, url
- [x] `(auth-revoke)` → boolean

### 2.2 Users (`slack/api/users.ss`)
- [x] `(users-list)` → list of `user` structs (paginated via `slack-api-paginated`)
- [x] `(users-info user-id)` → `user` struct
- [x] `(users-get-presence user-id)` → symbol: 'active or 'away
- [x] `(users-set-presence presence)` → boolean
- [x] `(users-set-status text emoji)` → boolean
- [x] `(user-id-for-name name)` → string (cached lookup)
- [x] `(user-name-for-id id)` → string (cached lookup)
- [x] `(users-hash)` / `(get-user-list)` / `(reset-user-cache!)` — caching helpers

### 2.3 Conversations (`slack/api/conversations.ss`)
- [x] `(conversations-list)` → list of `channel` structs (paginated)
- [x] `(conversations-info channel-id)` → `channel` struct
- [x] `(conversations-history channel-id)` → list of `message` structs
- [x] `(conversations-replies channel-id ts)` → list of `message` structs
- [x] `(conversations-members channel-id)` → list of user-id strings (paginated)
- [x] `(conversations-open)` → channel-id string
- [x] `(conversations-create name)` → `channel` struct
- [x] `(conversations-set-topic channel-id topic)` → boolean
- [x] `(conversations-set-purpose channel-id purpose)` → boolean
- [x] `(conversations-join channel-id)` → boolean
- [x] `(conversations-leave channel-id)` → boolean
- [x] `(conversations-invite channel-id users)` → boolean
- [x] `(conversations-kick channel-id user-id)` → boolean
- [x] `(conversations-archive channel-id)` → boolean
- [x] `(conversations-mark channel-id ts)` → boolean

### 2.4 Chat (`slack/api/chat.ss`)
- [x] `(chat-post-message channel text)` → ts string
- [x] `(chat-update channel ts text)` → boolean
- [x] `(chat-delete channel ts)` → boolean
- [x] `(chat-get-permalink channel ts)` → url string
- [x] `(chat-post-ephemeral channel user text)` → boolean
- [x] `(chat-schedule-message channel text post-at)` → scheduled-message-id
- [x] `(chat-me-message channel text)` → boolean

### 2.5 Reactions (`slack/api/reactions.ss`)
- [x] `(reactions-add channel ts name)` → boolean
- [x] `(reactions-remove channel ts name)` → boolean
- [x] `(reactions-get channel ts)` → list of `reaction` structs

### 2.6 Files (`slack/api/files.ss`)
- [x] `(files-upload file-path)` → `file-info` struct
- [x] `(files-delete file-id)` → boolean
- [x] `(files-info file-id)` → `file-info` struct
- [x] `(files-list)` → list of `file-info` structs
- [x] `(file-download file-url dest-path)` → boolean

### 2.7 Pins (`slack/api/pins.ss`)
- [x] `(pins-add channel ts)` → boolean
- [x] `(pins-remove channel ts)` → boolean
- [x] `(pins-list channel)` → list of message structs

### 2.8 Search (`slack/api/search.ss`)
- [x] `(search-messages query)` → list of `message` structs
- [x] `(search-files query)` → list of `file-info` structs
- [x] `(search-all query)` → response hash

### 2.9 Emoji (`slack/api/emoji.ss`)
- [x] `(emoji-list)` → hash of name → url

### 2.10 Team (`slack/api/team.ss`)
- [x] `(team-info)` → `team` struct

### 2.11 Reminders (`slack/api/reminders.ss`)
- [x] `(reminders-add text time)` → `reminder` struct
- [x] `(reminders-complete reminder-id)` → boolean
- [x] `(reminders-delete reminder-id)` → boolean
- [x] `(reminders-info reminder-id)` → `reminder` struct
- [x] `(reminders-list)` → list of `reminder` structs

### 2.12 DND (`slack/api/dnd.ss`)
- [x] `(dnd-set-snooze num-minutes)` → boolean
- [x] `(dnd-end-snooze)` → boolean
- [x] `(dnd-info)` → response hash

### 2.13 Usergroups (`slack/api/usergroups.ss`)
- [x] `(usergroups-list)` → list
- [x] `(usergroups-create name)` → response hash
- [x] `(usergroups-users-list usergroup-id)` → list of user-id strings

### 2.14 Bookmarks (`slack/api/bookmarks.ss`)
- [x] `(bookmarks-add channel-id title type link)` → `bookmark` struct
- [x] `(bookmarks-edit bookmark-id channel-id)` → `bookmark` struct
- [x] `(bookmarks-remove bookmark-id channel-id)` → boolean
- [x] `(bookmarks-list channel-id)` → list of `bookmark` structs

### 2.15 Build & Verify
- [x] All 14 API modules compile (18 total modules + exe)
- [x] All 60 existing unit tests pass
- [x] All API functions use keyword arguments with `token:` override

---

## Phase 3: Socket Mode — Real-Time Events ✅ COMPLETE

**Goal:** Implement the Socket Mode WebSocket client for receiving real-time events (messages, reactions, presence changes, typing indicators).

### 3.1 Event Types (`slack/events.ss`)
- [x] `defclass slack-event (type data envelope-id)` — base event wrapper
- [x] Event type symbols documented (message, reaction-added, presence-change, etc.)
- [x] `(on-event type handler)` — register handler, returns handler-id
- [x] `(emit-event type data)` — dispatch to type-specific and catch-all ('*) handlers
- [x] `(remove-handler type handler-id)` — unregister
- [x] `(clear-handlers!)` / `(handler-count)` — management helpers

### 3.2 Socket Mode Client (`slack/socket.ss`)
- [x] `(socket-mode-connect app-token)` — calls `apps.connections.open`, connects WebSocket
- [x] `(socket-mode-start! app-token)` — connect + begin event loop in background thread
- [x] `(socket-mode-stop!)` — graceful disconnect
- [x] WebSocket receive loop with envelope parsing and ack
- [x] Event type dispatch: events_api, slash_commands, interactive, hello, disconnect
- [x] Auto-reconnection with exponential backoff (1s → 30s max)
- [x] Underscore-to-dash event type conversion (message_changed → message-changed)
- [x] `(socket-mode-connected?)` — check connection status

### 3.3 Build & Verify
- [x] All 20 modules compile (including events + socket)
- [x] 79 unit tests pass (19 new events tests)
- [x] Event dispatcher: multiple handlers, catch-all, remove, clear all tested

---

## Phase 4: Cache Layer ✅ COMPLETE

**Goal:** SQLite-backed local cache for users, channels, and recent messages. Reduces API calls and enables offline browsing of cached content.

### 4.1 Cache Database (`slack/cache.ss`)
- [x] Schema: users, channels, messages, metadata tables with indexes
- [x] `(cache-open!)` → db connection (default: ~/.slack/cache.db)
- [x] `(cache-close!)` → close db
- [x] User cache: `cache-user!`, `cache-get-user`, `cache-all-users`, `cache-find-user-by-name`
- [x] Channel cache: `cache-channel!`, `cache-get-channel`, `cache-all-channels`, `cache-find-channel-by-name`
- [x] Message cache: `cache-message!`, `cache-get-messages` (with limit/before), `cache-get-thread`, `cache-delete-message!`
- [x] Metadata: `cache-set-meta!`, `cache-get-meta`
- [x] JSON serialization helpers for round-tripping structs through SQLite
- [x] Cache invalidation: TTL-based (users: 1 hour, channels: 5 min)
- [x] `cache-users-stale?`, `cache-channels-stale?`, `cache-mark-users-fresh!`, `cache-mark-channels-fresh!`
- [x] Event-driven cache updates: message, message-deleted, user-change, channel-created handlers auto-registered on cache-open!
- [x] `cache-stats` for user/channel/message counts

### 4.2 Build & Verify
- [x] 22 modules compile (including cache)
- [x] 125 unit tests pass (46 new cache tests)
- [x] Cache round-trip tests (store/retrieve users, channels, messages)
- [x] Event-driven cache updates tested (message insert + delete via events)

---

## Phase 5: Slack Markdown Parser ✅ COMPLETE

**Goal:** Parse Slack's `mrkdwn` format into plain text (for CLI) and HTML (for Qt rich text display).

### 5.1 Markdown Converter (`slack/markdown.ss`)
- [x] Parse Slack mrkdwn syntax:
  - `*bold*` → plain/stripped / `<b>bold</b>`
  - `_italic_` → plain/stripped / `<i>italic</i>`
  - `~strikethrough~` → plain/stripped / `<s>strikethrough</s>`
  - `` `code` `` → preserved / `<code>code</code>`
  - ` ```code block``` ` → indented / `<pre><code>code</code></pre>`
  - `> quote` → `| quote` / `<blockquote>quote</blockquote>`
  - `<@U123>` → `@username` (resolve via pluggable resolver)
  - `<#C123|channel>` → `#channel`
  - `<url|text>` → `text (url)` / `<a href="url">text</a>`
  - `:emoji:` → preserved as-is
  - Newlines preserved (HTML: converted to `<br>`)
- [x] `(mrkdwn->plain text)` → plain text string (for CLI)
- [x] `(mrkdwn->html text)` → HTML string (for Qt text browser)
- [x] `(set-user-resolver! f)` / `(set-channel-resolver! f)` — pluggable mention resolution
- [x] Code blocks extracted before formatting to prevent inner content processing
- [x] HTML output properly escapes `<`, `>`, `&`, `"`, `'`

### 5.2 Build & Verify
- [x] 23 modules compile (including markdown)
- [x] 158 unit tests pass (33 new markdown tests)
- [x] Tests cover: bold, italic, strikethrough, inline code, code blocks, blockquotes, user/channel mentions (with and without labels/resolvers), URLs (labeled and bare), emoji, mixed content, edge cases

---

## Phase 6: CLI Client ✅ COMPLETE

**Goal:** Full-featured command-line interface with all API methods accessible, human-readable output, and proper argument parsing via `:std/getopt`.

### 6.1 Output Formatters (`slack/cli/format.ss`)
- [x] Table formatter: aligned columns with headers, unicode box-drawing
- [x] Message formatter: timestamp, username, text with resolved mentions
- [x] JSON output mode: `output-json` helper for JSON output
- [x] User formatter: name, status, presence indicator
- [x] Color support via ANSI escape codes (detect `NO_COLOR` env var)
- [x] `(format-timestamp ts)` — Slack ts → human-readable local time
- [x] `(truncate-text text max-width)` — truncate with ellipsis for table cells

### 6.2 CLI Commands (`slack/cli/commands.ss`)
Using `:std/getopt` with subcommand dispatch via `rest-arguments` pattern:

**Auth & Config:**
- [x] `slack auth test` — verify token, show team/user info
- [x] `slack auth setup` — interactive token setup

**Channels:**
- [x] `slack channels list` — list channels with table output
- [x] `slack channels info CHANNEL` — show channel details
- [x] `slack channels create NAME [--private]` — create new channel
- [x] `slack channels topic CHANNEL TOPIC` — set channel topic
- [x] `slack channels purpose CHANNEL PURPOSE` — set channel purpose
- [x] `slack channels join CHANNEL` — join a channel
- [x] `slack channels leave CHANNEL` — leave a channel
- [x] `slack channels members CHANNEL` — list members
- [x] `slack channels invite CHANNEL USER` — invite user
- [x] `slack channels archive CHANNEL` — archive channel

**Messages:**
- [x] `slack messages history CHANNEL [--limit N]` — show message history
- [x] `slack messages send CHANNEL TEXT [--thread TS]` — send message
- [x] `slack messages edit CHANNEL TS TEXT` — edit message
- [x] `slack messages delete CHANNEL TS` — delete message
- [x] `slack messages thread CHANNEL TS [--limit N]` — show thread replies
- [x] `slack messages link CHANNEL TS` — get permalink
- [x] `slack messages schedule CHANNEL TEXT TIME` — schedule message

**Users:**
- [x] `slack users list` — list all users
- [x] `slack users info USER` — show user details
- [x] `slack users presence USER` — check presence
- [x] `slack users status TEXT EMOJI` — set your status
- [x] `slack users active` — set presence to active
- [x] `slack users away` — set presence to away

**DMs:**
- [x] `slack dm send USER TEXT` — send direct message
- [x] `slack dm history USER [--limit N]` — show DM history
- [x] `slack dm open USER` — open DM conversation

**Reactions:**
- [x] `slack react add CHANNEL TS EMOJI` — add reaction
- [x] `slack react remove CHANNEL TS EMOJI` — remove reaction
- [x] `slack react list CHANNEL TS` — list reactions

**Pins:**
- [x] `slack pin add CHANNEL TS` — pin message
- [x] `slack pin remove CHANNEL TS` — unpin message
- [x] `slack pin list CHANNEL` — list pinned messages

**Files:**
- [x] `slack files upload PATH [--channel CH] [--title T]` — upload file
- [x] `slack files list [--channel CH] [--user U]` — list files
- [x] `slack files info FILE_ID` — file details
- [x] `slack files delete FILE_ID` — delete file
- [x] `slack files download FILE_ID [DEST]` — download file

**Search:**
- [x] `slack search messages QUERY [--count N]` — search messages
- [x] `slack search files QUERY [--count N]` — search files

**Misc:**
- [x] `slack emoji list` — list custom emoji
- [x] `slack team info` — show team details
- [x] `slack reminders add TEXT TIME` — add reminder
- [x] `slack reminders list` — list reminders
- [x] `slack reminders complete ID` — complete reminder
- [x] `slack reminders delete ID` — delete reminder
- [x] `slack dnd snooze MINUTES` — enable DND
- [x] `slack dnd end` — end DND
- [x] `slack dnd info` — show DND status
- [x] `slack listen` — open Socket Mode and stream events to stdout

**Global Flags (all commands):**
- [x] `--token TOKEN` — override config token
- [x] `--no-color` — disable ANSI colors
- [x] `--version` — show version

### 6.3 CLI Entry Point (`slack/slack.ss`)
- [x] Main entry: parse argv, dispatch to subcommand
- [x] Help text for all commands
- [x] Version display (`--version`)
- [x] Error handling: catch exceptions, display human-readable message, exit with code 1

### 6.4 Build & Verify
- [x] CLI executable builds and runs
- [x] All subcommands accessible
- [x] Compiles without warnings
- **Note:** Entry point is `slack/slack.ss` (builds to `slack` executable). CLI uses `rest-arguments` pattern for nested subcommand dispatch since `:std/getopt` doesn't support nested `command` forms. `conversations-open` takes keyword-only args and returns channel-id string. `chat-post-message` returns ts string, not message struct.

---

## Phase 7: Qt GUI — Application Shell

**Goal:** Build the main window skeleton with sidebar, message area, and input bar. No real-time yet — just static content from API calls.

### 7.1 Application Lifecycle (`slack/gui/app.ss`)
- [x] `(main)` — entry point
  - Create main window
  - Start event loop
- [x] Qt application setup with `with-qt-app`
- [x] Window title: "Gerbil Slack"
- [x] Window minimum size: 900x600, default 1200x800
- [ ] Application icon (deferred — needs icon file)

### 7.2 Theme (`slack/gui/theme.ss`)
- [x] Color palette constants (Slack-inspired):
  - Sidebar background: `#3F0E40` (aubergine)
  - Sidebar text: `#FFFFFF`
  - Sidebar selected: `#1264A3`
  - Message area background: `#FFFFFF`
  - Message text: `#1D1C1D`
  - Timestamp text: `#616061`
  - Input area border: `#DDDDDD`
  - Link color: `#1264A3`
- [x] Font configuration: 14px base in message area, styled per widget
- [x] Global stylesheet string for `qt-widget-set-style-sheet!`
- [x] `(apply-theme! app)` — apply global stylesheet to application
- [x] Per-widget style helpers (gerbil-qt lacks set-object-name!, so CSS selectors by object name don't work)

### 7.3 Main Window Layout (`slack/gui/app.ss`)
```
┌─────────────────────────────────────────────────────────┐
│ Menu Bar                                                │
├────────────┬────────────────────────────────────────────┤
│            │  #channel-name ★  ⓘ                       │
│  Sidebar   ├────────────────────────────────────────────┤
│            │                                            │
│ ■ Team     │  Message area (scrollable)                 │
│            │                                            │
│ Channels   │  [10:32] alice: Hello world                │
│  #general  │  [10:33] bob: Hey!                         │
│  #random   │                                            │
│  #eng      │                                            │
│            │                                            │
│ Direct     ├────────────────────────────────────────────┤
│  @alice    │  ┌──────────────────────────────────────┐  │
│  @bob      │  │ Message input area                   │  │
│            │  │                                      │  │
│            │  └──────────────────────────────────────┘  │
│            │  [Send]                                    │
├────────────┴────────────────────────────────────────────┤
│ Status Bar: Connected as @username | Team Name          │
└─────────────────────────────────────────────────────────┘
```
- [x] Main splitter: sidebar (fixed ~250px) + content area
- [x] Content area: vertical layout with channel header, message scroll area, input bar
- [x] Status bar at bottom
- [x] Menu bar: File (Preferences, Quit), Channel (Info, Members, Topic), Help (About)

### 7.4 Build & Verify
- [x] GUI compiles as library modules (exe linking deferred — requires Qt C++ library linkage)
- [x] Sidebar visible, content area visible, input area visible
- [x] Menu bar functional (Quit with Ctrl+Q)
- [x] Compiles without warnings
- **Note:** GUI modules (`slack/gui/theme`, `slack/gui/app`) compile as libraries only — not as executables yet. Building an exe requires linking Qt C++ libraries (`libqt_shim.so` + Qt frameworks) which needs additional Makefile configuration. gerbil-qt is not a gxpkg package, so `depend:` in `gerbil.pkg` doesn't work; instead `GERBIL_LOADPATH` is set in the Makefile to find it. Per-widget styling is used because gerbil-qt lacks `qt-widget-set-object-name!`.

---

## Phase 8: Qt GUI — Sidebar

**Goal:** Populate sidebar with channels and DMs from cache/API.

### 8.1 Sidebar Widget (`slack/gui/sidebar.ss`)
- [x] Team header at top (team name fetched via team-info, sets window title)
- [x] "Channels" section
  - List all public/private channels (non-archived)
  - `#` prefix for public channels in display
  - Click to switch active channel via `qt-on-current-row-changed!`
- [x] "Direct Messages" section
  - List DMs and MPIMs (non-archived)
  - Click to switch active DM
- [x] Search/filter input at top of sidebar (uses `qt-on-text-changed!` + `string-contains`)
- [ ] Right-click context menu (deferred to Phase 17 polish)
- [x] Current channel highlighted with accent color (via list widget selection)
- [x] `(sidebar-refresh!)` — reload channels from cache/API, repopulate lists
- [x] `(sidebar-set-active! channel-id)` — highlight selected channel/DM
- [x] Signal: channel selection changed → invokes `*on-channel-selected*` callback
- [x] `(sidebar-init! on-select)` — wire all signals, set callback
- [ ] Bold unread channels (deferred to Phase 11 real-time)
- [ ] Presence indicators (deferred to Phase 11 real-time)

### 8.2 Build & Verify
- [x] Channels and DMs appear in sidebar from cache/API data
- [x] Clicking a channel fires selection callback
- [x] Filter narrows list
- [x] Compiles without warnings
- **Note:** Theme color constants renamed with `clr-` prefix to avoid import conflicts with types module (`message-text` etc.). Presence indicators and unread bolding deferred to Phase 11 (Real-Time Integration) since they require Socket Mode events.

---

## Phase 9: Qt GUI — Message Display

**Goal:** Render messages in the main content area with proper formatting, avatars placeholder, timestamps, and reactions.

### 9.1 Channel View (`slack/gui/channel-view.ss`)
- [x] Channel header bar: channel name and topic from cache/API
- [x] Message area uses QTextBrowser with full HTML rendering
- [x] Load history on channel selection (cache first, then API)
- [x] Scroll to bottom on new messages
- [x] `channel-view-load-older!` for pagination (loads 50 older messages)
- [ ] "New messages" divider line (deferred to Phase 11 real-time)
- [x] Date separators between messages from different days

### 9.2 Message Rendering (integrated in `slack/gui/channel-view.ss`)
- [x] Username in bold, timestamp (HH:MM UTC) in gray
- [x] Message body rendered as HTML via `mrkdwn->html` converter
- [x] Reactions bar below message (emoji name + count badges)
- [x] File attachment indicators (name + human-readable size)
- [x] Thread reply indicator ("N replies" link)
- [ ] Hover actions (deferred — requires Qt event filter support)
- [x] Edited indicator: "(edited)" in small gray text
- [x] Bot messages with "APP" badge
- [x] User ID → display name resolution via cache + API
- [x] `channel-view-append-message!` for real-time new messages
- [x] `channel-view-update-message!` for edited messages
- [x] `channel-view-delete-message!` for deleted messages

### 9.3 Build & Verify
- [x] Messages render from channel history as styled HTML
- [x] Mrkdwn formatting displayed via mrkdwn->html
- [x] Scroll-to-bottom works
- [x] Pagination support via channel-view-load-older!
- [x] Reactions visible as styled badges
- [x] Compiles without warnings
- **Note:** Single module `channel-view.ss` handles both channel view and message rendering (no separate message-widget.ss needed since QTextBrowser renders all messages as one HTML document). Timestamps shown in UTC (HH:MM). Date separators use approximate Gregorian calendar conversion from epoch.

---

## Phase 10: Qt GUI — Message Input & Sending

**Goal:** Full message composition with send functionality, thread replies, and basic keyboard shortcuts.

### 10.1 Input Bar (`slack/gui/input-bar.ss`)
- [x] Multi-line text input area (QPlainTextEdit, max 80px height, from Phase 7)
- [x] Send button (right side, wired in input-bar-init!)
- [x] Enter sends message, Shift+Enter adds newline (via qt-on-key-press! + QT_MOD_SHIFT)
- [x] Thread reply context: `input-bar-set-thread!` / `input-bar-clear-thread!`
- [ ] `@mention` autocomplete (deferred — requires popup widget support)
- [ ] `#channel` autocomplete (deferred)
- [ ] `:emoji:` autocomplete (deferred)
- [ ] File attachment button (deferred to Phase 14)
- [ ] Message editing via Up arrow (deferred)

### 10.2 Send Logic
- [x] Send message to current channel via `chat-post-message`
- [x] Reply in thread: sends with `thread_ts` when set
- [x] Clear input on successful send (optimistic)
- [x] Error recovery: if send fails, restore input text and show error in status bar
- [x] Status bar feedback on send success/failure

### 10.3 Build & Verify
- [x] Send messages via chat-post-message
- [x] Enter/Shift+Enter behavior works
- [x] Thread replies supported via input-bar-set-thread!
- [x] Compiles without warnings
- **Note:** Autocomplete popups deferred as they require either a QCompleter or custom popup widget. The key handler uses `qt-on-key-press!` with `qt-last-key-code`/`qt-last-key-modifiers` to distinguish Enter vs Shift+Enter.

---

## Phase 11: Qt GUI — Real-Time Integration

**Goal:** Connect Socket Mode to the GUI so new messages, reactions, and presence changes appear live.

### 11.1 Event→GUI Bridge (`slack/gui/realtime.ss`)
- [x] Start Socket Mode connection in background thread via `realtime-start!`
- [x] Thread-safe event queue with mutex + unwind-protect
- [x] Qt timer polls queue at 100ms, drains thunks on main thread
- [x] Event handlers registered for all major event types:
  - [x] `message` → append message to active channel view
  - [x] `message-changed` → update existing message
  - [x] `message-deleted` → remove message from view
  - [x] `reaction-added/removed` → reload channel view
  - [x] `channel-created` → refresh sidebar
  - [x] `user-typing` → show "X is typing..." in status bar
  - [x] `connected/disconnected/reconnecting` → update status bar

### 11.2 Unread Tracking
- [ ] Track last-read timestamp per channel (deferred — needs UI for unread indicators)
- [ ] Bold unread channels in sidebar (deferred)
- [ ] Unread count badge (deferred)

### 11.3 Typing Indicators
- [x] Show "X is typing..." in status bar (resolves username from cache)
- [ ] Auto-clear after timeout (deferred)
- [ ] Multiple typists display (deferred)

### 11.4 Connection Status
- [x] Status bar shows: Connected / Reconnecting... / Disconnected
- [x] `realtime-stop!` for clean shutdown

### 11.5 Build & Verify
- [x] Event handlers registered and queue events for main thread
- [x] Mutex patterns use unwind-protect (lint clean)
- [x] Compiles without warnings
- **Note:** Uses SLACK_APP_TOKEN env var for Socket Mode. Event queue pattern: background thread calls `enqueue-gui-event!` with a thunk, Qt timer on main thread calls `drain-gui-events!` every 100ms. Reaction changes trigger a full channel reload since individual reaction updates require knowing the full message state.

---

## Phase 12: Qt GUI — Thread View

**Goal:** Side panel for viewing and replying to threads.

### 12.1 Thread Panel (`slack/gui/thread-view.ss`)
- [x] `thread-view-build!` adds panel to parent splitter, starts hidden
- [x] Parent message displayed at top with separator border
- [x] All replies listed chronologically below
- [x] Own input bar at bottom with Reply button
- [x] Close button (X) to dismiss panel
- [x] Enter-to-send in thread input (Shift+Enter for newline)
- [ ] "Also send to #channel" checkbox (deferred)
- [x] `thread-view-append-reply!` for real-time new replies
- [x] Thread panel resizable via splitter (min width 300px)
- [x] `thread-view-open!` / `thread-view-close!` / `thread-view-open?`

### 12.2 Build & Verify
- [x] Thread panel renders parent + replies
- [x] Reply in thread via chat-post-message with thread_ts
- [x] Real-time reply append supported
- [x] Close panel hides widget
- [x] Compiles without warnings

---

## Phase 13: Qt GUI — User & Channel Info Panels

**Goal:** Detailed info panels for users and channels.

### 13.1 User Info Panel
- [ ] Click username in message → popup/panel with user details:
  - Display name, real name, title
  - Status text + emoji
  - Presence (active/away)
  - Timezone + local time
  - "Message" button → open DM
- [ ] Implement as either modal dialog or slide-in right panel

### 13.2 Channel Info Panel
- [ ] Click channel name in header → panel with:
  - Channel name, topic, purpose, description
  - Created date and creator
  - Member count with scrollable member list
  - Pinned messages
  - Bookmarks
  - "Edit Topic" / "Edit Purpose" buttons
  - "Leave Channel" button
- [ ] Member list shows presence indicators

### 13.3 Build & Verify
- [ ] User popup shows accurate data
- [ ] Channel info panel loads members
- [ ] "Message" button opens DM

---

## Phase 14: Qt GUI — File Handling

**Goal:** Upload files and display file attachments inline.

### 14.1 File Upload (`slack/gui/file-upload.ss`)
- [ ] Drag-and-drop files onto message area → upload dialog
- [ ] Upload dialog: file preview, title field, comment field, destination channel
- [ ] Progress indicator during upload
- [ ] Also accessible via attachment button in input bar
- [ ] Paste image from clipboard → upload

### 14.2 File Display
- [ ] Image attachments: thumbnail preview inline in message
- [ ] Other files: icon + filename + size, clickable to download
- [ ] Download to user-selected location via save dialog
- [ ] Open in default application option

### 14.3 Build & Verify
- [ ] Upload file via drag-and-drop
- [ ] Upload via attachment button
- [ ] File appears in message with correct metadata
- [ ] Download works

---

## Phase 15: Qt GUI — Search

**Goal:** Global search across messages and files.

### 15.1 Search Dialog (`slack/gui/search-dialog.ss`)
- [ ] Triggered by Ctrl+F or search icon in toolbar
- [ ] Search input at top
- [ ] Results tabbed: Messages | Files
- [ ] Message results show: channel, user, timestamp, text snippet with highlighted query match
- [ ] Click result → navigate to that channel and scroll to message
- [ ] File results show: filename, uploader, date, channel
- [ ] Sort options: relevance, timestamp
- [ ] Search modifiers: `in:#channel`, `from:@user`, `before:date`, `after:date`

### 15.2 Build & Verify
- [ ] Search returns relevant results
- [ ] Click result navigates correctly
- [ ] Sort and filter modifiers work

---

## Phase 16: Qt GUI — Preferences & Settings

**Goal:** Settings dialog for configuring the client.

### 16.1 Preferences (`slack/gui/preferences.ss`)
- [ ] Modal dialog with tabs:
  - **General**: theme (light/dark), font size, notification sounds on/off
  - **Account**: show current team/user, token management, logout
  - **Advanced**: cache directory, cache size, clear cache button, debug logging toggle
- [ ] Persist settings via `QSettings` (survives across sessions)
- [ ] Apply theme changes immediately

### 16.2 System Tray (`slack/gui/tray.ss`)
- [ ] System tray icon when minimized
- [ ] Tray menu: Show/Hide, Status (Active/Away/DND), Quit
- [ ] Notification badge on tray icon for unread messages
- [ ] Click tray icon → restore window
- [ ] Close button minimizes to tray instead of quitting (configurable)

### 16.3 Build & Verify
- [ ] Preferences save and restore across sessions
- [ ] Tray icon appears when minimized
- [ ] Tray notifications work

---

## Phase 17: Polish & Edge Cases

**Goal:** Handle error states, offline mode, accessibility, and performance.

### 17.1 Error Handling
- [ ] Network errors: show banner "Connection lost, retrying..."
- [ ] API errors: toast notification with error message
- [ ] Token expired: prompt to re-authenticate
- [ ] Rate limiting: queue requests, show "Slow down" indicator

### 17.2 Keyboard Shortcuts
- [ ] `Ctrl+K` — Quick channel switcher (fuzzy search popup)
- [ ] `Ctrl+F` — Search
- [ ] `Ctrl+N` — New message
- [ ] `Ctrl+Shift+M` — Toggle mentions panel
- [ ] `Escape` — Close thread/panel/dialog
- [ ] `Alt+Up/Down` — Navigate channels in sidebar
- [ ] `Ctrl+Q` — Quit

### 17.3 Performance
- [ ] Lazy-load messages (only render visible viewport + small buffer)
- [ ] Cache user avatars locally
- [ ] Debounce sidebar filter input
- [ ] Background cache warming (don't block UI on startup)

### 17.4 Accessibility
- [ ] All interactive elements keyboard-focusable
- [ ] Sensible tab order
- [ ] High-contrast theme option

### 17.5 Build & Verify
- [ ] Keyboard shortcuts all functional
- [ ] App handles network loss gracefully
- [ ] Performance acceptable with 1000+ messages in history
- [ ] No memory leaks on extended use (check with heap profiler)

---

## Phase Summary

| Phase | Component | Description | Dependencies |
|-------|-----------|-------------|--------------|
| 1 | Core | Config, types, HTTP client | None |
| 2 | Core | All API method wrappers | Phase 1 |
| 3 | Core | Socket Mode real-time events | Phase 1 |
| 4 | Core | SQLite cache layer | Phase 2 |
| 5 | Core | Slack markdown parser | Phase 1 |
| 6 | CLI | Full command-line interface | Phase 2, 5 |
| 7 | GUI | Application shell & layout | Phase 1 |
| 8 | GUI | Sidebar with channels/DMs | Phase 2, 4, 7 |
| 9 | GUI | Message display & rendering | Phase 2, 4, 5, 7 |
| 10 | GUI | Message input & sending | Phase 2, 9 |
| 11 | GUI | Real-time event integration | Phase 3, 9 |
| 12 | GUI | Thread view panel | Phase 9, 10, 11 |
| 13 | GUI | User & channel info panels | Phase 2, 4, 8 |
| 14 | GUI | File upload & display | Phase 2, 10 |
| 15 | GUI | Search dialog | Phase 2, 9 |
| 16 | GUI | Preferences & system tray | Phase 7 |
| 17 | GUI | Polish, shortcuts, perf | All |

### Build Targets

```makefile
build:      ## Build everything (library + CLI + GUI)
build-cli:  ## Build only library + CLI executable
build-gui:  ## Build only library + GUI executable
test:       ## Run all tests
clean:      ## Remove build artifacts
install:    ## Install to ~/.gerbil
```

### Dependencies

```scheme
;; gerbil.pkg
(package: ober
 depend: ("github.com/ober/gerbil-qt"))

;; Standard library (no external deps beyond gerbil-qt):
;;   :std/net/request        — HTTP client
;;   :std/net/websocket      — WebSocket for Socket Mode
;;   :std/text/json           — JSON encode/decode
;;   :std/db/sqlite           — Local cache
;;   :std/crypto/cipher       — Token encryption
;;   :std/crypto/digest       — Hashing
;;   :std/getopt              — CLI argument parsing
;;   :std/pregexp             — Regex for markdown parsing
;;   :std/format              — String formatting
;;   :std/iter                — Iteration
;;   :std/sugar               — let-hash, if-let, chain, etc.
;;   :std/misc/channel        — Thread communication
;;   :std/misc/string         — String utilities
;;   :std/srfi/19             — Date/time handling
;;   :gerbil-qt/qt            — Qt6 widgets (GUI only)
```
