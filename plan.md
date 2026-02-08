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

## Phase 3: Socket Mode — Real-Time Events

**Goal:** Implement the Socket Mode WebSocket client for receiving real-time events (messages, reactions, presence changes, typing indicators).

### 3.1 Event Types (`slack/events.ss`)
- [ ] `defstruct slack-event (type data envelope-id)` — base event wrapper
- [ ] Define event type symbols:
  - `message` — new message in channel/DM
  - `message-changed` — message edited
  - `message-deleted` — message deleted
  - `reaction-added` / `reaction-removed`
  - `channel-created` / `channel-archived` / `channel-unarchive`
  - `member-joined-channel` / `member-left-channel`
  - `user-typing`
  - `presence-change`
  - `team-join` — new user joined workspace
  - `emoji-changed`
  - `file-shared` / `file-deleted`
  - `pin-added` / `pin-removed`
  - `user-status-changed`
- [ ] Event dispatcher: `(on-event type handler)` — register handler for event type
- [ ] `(emit-event type data)` — dispatch to registered handlers
- [ ] `(remove-handler type handler-id)` — unregister

### 3.2 Socket Mode Client (`slack/socket.ss`)
- [ ] `(socket-mode-connect app-token)` — calls `apps.connections.open`, gets WSS URL
- [ ] `(socket-mode-start! app-token)` — connect + begin event loop in background thread
- [ ] `(socket-mode-stop!)` — graceful disconnect
- [ ] WebSocket event loop:
  - Receive JSON frames via `WebSocket-recv`
  - Parse envelope: extract `envelope_id`, `type`, `payload`
  - Send acknowledgement: `{"envelope_id": "...", "payload": {}}`
  - Dispatch `payload.event` to event system
- [ ] Auto-reconnection with exponential backoff (1s, 2s, 4s, 8s, max 30s)
- [ ] Heartbeat/ping handling to detect stale connections
- [ ] Thread-safe event dispatch (events arrive on WS thread, handlers may run on UI thread)

### 3.3 Build & Verify
- [ ] Socket mode connects and receives events
- [ ] Test: send message in Slack UI → event received in client
- [ ] Test: reconnection after network interruption
- [ ] Test: multiple event handlers for same event type

---

## Phase 4: Cache Layer

**Goal:** SQLite-backed local cache for users, channels, and recent messages. Reduces API calls and enables offline browsing of cached content.

### 4.1 Cache Database (`slack/cache.ss`)
- [ ] Schema:
  ```sql
  CREATE TABLE users (id TEXT PRIMARY KEY, name TEXT, real_name TEXT, display_name TEXT,
                      email TEXT, avatar_url TEXT, is_bot INTEGER, json TEXT, updated_at INTEGER);
  CREATE TABLE channels (id TEXT PRIMARY KEY, name TEXT, is_channel INTEGER, is_im INTEGER,
                         is_private INTEGER, is_archived INTEGER, json TEXT, updated_at INTEGER);
  CREATE TABLE messages (ts TEXT, channel TEXT, user TEXT, text TEXT, thread_ts TEXT,
                         json TEXT, PRIMARY KEY (channel, ts));
  CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT);
  CREATE INDEX idx_messages_channel ON messages(channel, ts DESC);
  CREATE INDEX idx_messages_thread ON messages(channel, thread_ts, ts);
  ```
- [ ] `(cache-open! #!key (path "~/.slack/cache.db"))` → db connection
- [ ] `(cache-close!)` → close db
- [ ] User cache: `(cache-user! user)`, `(cache-get-user id)`, `(cache-all-users)`
- [ ] Channel cache: `(cache-channel! ch)`, `(cache-get-channel id)`, `(cache-all-channels)`
- [ ] Message cache: `(cache-message! msg)`, `(cache-get-messages channel-id #!key limit before)`
- [ ] Metadata: `(cache-set-meta! key value)`, `(cache-get-meta key)`
- [ ] Cache warming: `(cache-warm!)` — fetch and cache all users + channels on startup
- [ ] Cache invalidation: TTL-based (users: 1 hour, channels: 5 min, messages: on-demand)
- [ ] Wire up event system: new messages/edits/deletes update cache automatically

### 4.2 Build & Verify
- [ ] Cache round-trip tests (store/retrieve users, channels, messages)
- [ ] Cache warming test with live API
- [ ] Event-driven cache updates work

---

## Phase 5: Slack Markdown Parser

**Goal:** Parse Slack's `mrkdwn` format into plain text (for CLI) and HTML (for Qt rich text display).

### 5.1 Markdown Converter (`slack/markdown.ss`)
- [ ] Parse Slack mrkdwn syntax:
  - `*bold*` → **bold** / `<b>bold</b>`
  - `_italic_` → _italic_ / `<i>italic</i>`
  - `~strikethrough~` → ~~strikethrough~~ / `<s>strikethrough</s>`
  - `` `code` `` → `code` / `<code>code</code>`
  - ` ```code block``` ` → indented / `<pre>code block</pre>`
  - `>quote` → `| quote` / `<blockquote>quote</blockquote>`
  - `<@U123>` → `@username` (resolve via cache)
  - `<#C123|channel>` → `#channel`
  - `<url|text>` → `text (url)` / `<a href="url">text</a>`
  - `:emoji:` → emoji character or placeholder
  - Newlines preserved
- [ ] `(mrkdwn->plain text)` → plain text string (for CLI)
- [ ] `(mrkdwn->html text)` → HTML string (for Qt text browser)
- [ ] User/channel mention resolution using cache

### 5.2 Build & Verify
- [ ] Unit tests with sample Slack messages
- [ ] Edge cases: nested formatting, URLs with special chars, code blocks with mrkdwn chars inside

---

## Phase 6: CLI Client

**Goal:** Full-featured command-line interface with all API methods accessible, human-readable output, and proper argument parsing via `:std/getopt`.

### 6.1 Output Formatters (`slack/cli/format.ss`)
- [ ] Table formatter: aligned columns with headers, unicode box-drawing
  ```
  ┌──────────┬────────────────┬─────────┐
  │ ID       │ Name           │ Members │
  ├──────────┼────────────────┼─────────┤
  │ C01234   │ #general       │     142 │
  │ C05678   │ #engineering   │      38 │
  └──────────┴────────────────┴─────────┘
  ```
- [ ] Message formatter: timestamp, username, text with resolved mentions
  ```
  [10:32 AM] alice: Hey team, the deploy is done
  [10:33 AM] bob: Nice! Any issues?
  [10:35 AM] alice: Clean deploy, no errors :tada:
  ```
- [ ] JSON output mode: `--json` flag outputs raw JSON for piping
- [ ] User formatter: name, status, presence indicator
- [ ] Color support via ANSI escape codes (detect `NO_COLOR` env var)
- [ ] `(format-timestamp ts)` — Slack ts → human-readable local time
- [ ] `(truncate-text text max-width)` — truncate with ellipsis for table cells

### 6.2 CLI Commands (`slack/cli/commands.ss`)
Using `:std/getopt` with subcommand dispatch:

**Auth & Config:**
- [ ] `slack auth test` — verify token, show team/user info
- [ ] `slack auth setup` — interactive token setup
- [ ] `slack config show` — display current configuration

**Channels:**
- [ ] `slack channels list [--type TYPE]` — list channels (public, private, im, mpim)
- [ ] `slack channels info CHANNEL` — show channel details
- [ ] `slack channels create NAME [--private]` — create new channel
- [ ] `slack channels topic CHANNEL TOPIC` — set channel topic
- [ ] `slack channels purpose CHANNEL PURPOSE` — set channel purpose
- [ ] `slack channels join CHANNEL` — join a channel
- [ ] `slack channels leave CHANNEL` — leave a channel
- [ ] `slack channels members CHANNEL` — list members
- [ ] `slack channels invite CHANNEL USER` — invite user
- [ ] `slack channels archive CHANNEL` — archive channel

**Messages:**
- [ ] `slack messages history CHANNEL [--limit N] [--before TS]` — show message history
- [ ] `slack messages send CHANNEL TEXT [--thread TS]` — send message
- [ ] `slack messages edit CHANNEL TS TEXT` — edit message
- [ ] `slack messages delete CHANNEL TS` — delete message
- [ ] `slack messages thread CHANNEL TS [--limit N]` — show thread replies
- [ ] `slack messages link CHANNEL TS` — get permalink
- [ ] `slack messages schedule CHANNEL TEXT TIME` — schedule message

**Users:**
- [ ] `slack users list` — list all users
- [ ] `slack users info USER` — show user details
- [ ] `slack users presence USER` — check presence
- [ ] `slack users status TEXT EMOJI [--expiration TIME]` — set your status
- [ ] `slack users active` — set presence to active
- [ ] `slack users away` — set presence to away

**DMs:**
- [ ] `slack dm send USER TEXT` — send direct message
- [ ] `slack dm history USER [--limit N]` — show DM history
- [ ] `slack dm open USER` — open DM conversation

**Reactions:**
- [ ] `slack react add CHANNEL TS EMOJI` — add reaction
- [ ] `slack react remove CHANNEL TS EMOJI` — remove reaction
- [ ] `slack react list CHANNEL TS` — list reactions

**Pins:**
- [ ] `slack pin add CHANNEL TS` — pin message
- [ ] `slack pin remove CHANNEL TS` — unpin message
- [ ] `slack pin list CHANNEL` — list pinned messages

**Files:**
- [ ] `slack files upload PATH [--channel CH] [--title T] [--comment C]` — upload file
- [ ] `slack files list [--channel CH] [--user U]` — list files
- [ ] `slack files info FILE_ID` — file details
- [ ] `slack files delete FILE_ID` — delete file
- [ ] `slack files download FILE_ID [DEST]` — download file

**Search:**
- [ ] `slack search messages QUERY [--sort S] [--count N]` — search messages
- [ ] `slack search files QUERY [--sort S] [--count N]` — search files

**Misc:**
- [ ] `slack emoji list` — list custom emoji
- [ ] `slack team info` — show team details
- [ ] `slack reminders add TEXT TIME` — add reminder
- [ ] `slack reminders list` — list reminders
- [ ] `slack dnd snooze MINUTES` — enable DND
- [ ] `slack dnd end` — end DND
- [ ] `slack listen` — open Socket Mode and stream events to stdout (useful for scripting)

**Global Flags (all commands):**
- [ ] `--token TOKEN` — override config token
- [ ] `--json` — output raw JSON
- [ ] `--no-color` — disable ANSI colors
- [ ] `--verbose` — show HTTP request details

### 6.3 CLI Entry Point (`slack/cli.ss`)
- [ ] Main entry: parse argv, dispatch to subcommand
- [ ] Help text for all commands (`--help` / `-h`)
- [ ] Version display (`--version`)
- [ ] Error handling: catch `slack-error`, display human-readable message, exit with code 1
- [ ] Name resolution: accept channel names (`#general`) or IDs (`C01234`) interchangeably
- [ ] User resolution: accept usernames (`@alice`) or IDs (`U01234`) interchangeably

### 6.4 Build & Verify
- [ ] CLI executable builds and runs
- [ ] All subcommands accessible via `--help`
- [ ] Test each command category against live workspace
- [ ] JSON output mode works for piping to `jq`

---

## Phase 7: Qt GUI — Application Shell

**Goal:** Build the main window skeleton with sidebar, message area, and input bar. No real-time yet — just static content from API calls.

### 7.1 Application Lifecycle (`slack/gui/app.ss`)
- [ ] `(main)` — entry point
  - Load config (or launch setup wizard on first run)
  - Open cache database
  - Warm caches (users, channels)
  - Create main window
  - Start event loop
- [ ] Qt application setup with `with-qt-app`
- [ ] Window title: "Gerbil Slack — {team-name}"
- [ ] Window minimum size: 900x600, default 1200x800
- [ ] Application icon

### 7.2 Theme (`slack/gui/theme.ss`)
- [ ] Color palette constants (Slack-inspired):
  - Sidebar background: `#3F0E40` (aubergine)
  - Sidebar text: `#FFFFFF`
  - Sidebar selected: `#1264A3`
  - Message area background: `#FFFFFF`
  - Message text: `#1D1C1D`
  - Timestamp text: `#616061`
  - Input area border: `#DDDDDD`
  - Link color: `#1264A3`
  - Mention highlight: `#FFF3CD`
- [ ] Font configuration: system default, 14px base
- [ ] Global stylesheet string for `qt-widget-set-style-sheet!`
- [ ] `(apply-theme! widget)` — apply stylesheet to widget tree

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
- [ ] Main splitter: sidebar (fixed ~250px) + content area
- [ ] Content area: vertical layout with channel header, message scroll area, input bar
- [ ] Status bar at bottom
- [ ] Menu bar: File (Preferences, Quit), Edit (Copy, Find), Channel (Info, Members, Topic), Help (About)

### 7.4 Build & Verify
- [ ] GUI launches with empty shell layout
- [ ] Sidebar visible, content area visible, input area visible
- [ ] Window resizes properly
- [ ] Menu bar functional (Quit works)

---

## Phase 8: Qt GUI — Sidebar

**Goal:** Populate sidebar with channels and DMs from cache/API.

### 8.1 Sidebar Widget (`slack/gui/sidebar.ss`)
- [ ] Team header at top (team name + icon)
- [ ] "Channels" section with collapsible header
  - List all public/private channels user is member of
  - Bold unread channels
  - `#` prefix for public, `🔒` for private
  - Click to switch active channel
- [ ] "Direct Messages" section with collapsible header
  - List DMs sorted by recent activity
  - Show presence indicator (green dot = active, hollow = away)
  - Show user's display name
  - Click to switch active DM
- [ ] Search/filter input at top of sidebar
- [ ] Right-click context menu: Mark as Read, Mute, Leave Channel
- [ ] Current channel highlighted with accent color
- [ ] `(sidebar-refresh!)` — reload channels list from API/cache
- [ ] `(sidebar-set-active! channel-id)` — highlight selected channel
- [ ] Signal: channel selection changed → load messages for that channel

### 8.2 Build & Verify
- [ ] Channels and DMs appear in sidebar from live API data
- [ ] Clicking a channel fires selection signal
- [ ] Filter narrows list
- [ ] Presence dots display correctly

---

## Phase 9: Qt GUI — Message Display

**Goal:** Render messages in the main content area with proper formatting, avatars placeholder, timestamps, and reactions.

### 9.1 Channel View (`slack/gui/channel-view.ss`)
- [ ] Channel header bar: channel name, topic (truncated), member count, pin icon
- [ ] Scrollable message area using `QScrollArea` with vertical layout
- [ ] Load history on channel selection (from cache first, then API)
- [ ] Scroll to bottom on new messages
- [ ] Load older messages on scroll-to-top (pagination)
- [ ] "New messages" divider line when returning to channel with unreads
- [ ] Date separators between messages from different days

### 9.2 Message Widget (`slack/gui/message-widget.ss`)
- [ ] Individual message rendering:
  ```
  [Avatar]  Username                 10:32 AM
            Message text here with *bold* and _italic_
            rendered as rich text via mrkdwn→HTML converter

            📎 filename.pdf (2.3 MB)         [reactions: 👍 3  🎉 1]
  ```
- [ ] Avatar placeholder (colored circle with initial, or loaded image)
- [ ] Username in bold, timestamp in gray
- [ ] Message body rendered as HTML via `QTextBrowser` (supports links, code blocks)
- [ ] Reactions bar below message (emoji + count, clickable to toggle)
- [ ] File attachment indicators (name, size, download link)
- [ ] Thread reply indicator: "N replies — Last reply TIME →" (clickable)
- [ ] Hover actions: React 😀, Reply in Thread 🧵, More ⋯
- [ ] Edited indicator: "(edited)" in small gray text
- [ ] Bot messages with "APP" badge

### 9.3 Build & Verify
- [ ] Messages render from channel history
- [ ] Mrkdwn formatting displays correctly
- [ ] Scroll-to-bottom works
- [ ] Older messages load on scroll-up
- [ ] Reactions visible

---

## Phase 10: Qt GUI — Message Input & Sending

**Goal:** Full message composition with send functionality, thread replies, and basic keyboard shortcuts.

### 10.1 Input Bar (`slack/gui/input-bar.ss`)
- [ ] Multi-line text input area (`QTextEdit` with dynamic height, max ~5 lines)
- [ ] Send button (right side)
- [ ] Enter sends message, Shift+Enter adds newline
- [ ] Show "typing in #channel" or "replying in thread" label above input
- [ ] `@mention` autocomplete: type `@` → popup with user list, filter as you type
- [ ] `#channel` autocomplete: type `#` → popup with channel list
- [ ] `:emoji:` autocomplete: type `:` → popup with emoji names
- [ ] File attachment button (opens file picker → upload → attach to message)
- [ ] `/` command detection (future: slash command support)
- [ ] Message editing: Up arrow in empty input → edit last message you sent
- [ ] Character count / message preview

### 10.2 Send Logic
- [ ] Send message to current channel via `chat-post-message`
- [ ] Reply in thread: when thread view is active, send with `thread_ts`
- [ ] Clear input on successful send
- [ ] Optimistic display: show message immediately in UI, update ts when API confirms
- [ ] Error display: if send fails, show error inline and preserve input text

### 10.3 Build & Verify
- [ ] Type and send messages successfully
- [ ] Messages appear in channel after sending
- [ ] Enter/Shift+Enter behavior works
- [ ] Autocomplete popups show relevant suggestions
- [ ] Thread replies work

---

## Phase 11: Qt GUI — Real-Time Integration

**Goal:** Connect Socket Mode to the GUI so new messages, reactions, and presence changes appear live.

### 11.1 Event→GUI Bridge
- [ ] Start Socket Mode connection in background thread on app launch
- [ ] Route events to Qt main thread (must not update Qt widgets from background thread)
  - Use `QTimer` single-shot with 0ms to schedule GUI updates from event handlers
  - Or use a shared thread-safe queue polled by a periodic timer
- [ ] Event handlers:
  - `message` → append message widget to active channel (or increment unread badge)
  - `message-changed` → update existing message widget
  - `message-deleted` → remove or gray-out message widget
  - `reaction-added/removed` → update reaction bar on message
  - `presence-change` → update presence dot in sidebar
  - `user-typing` → show "X is typing..." indicator
  - `channel-created` → add to sidebar
  - `member-joined/left` → update member count

### 11.2 Unread Tracking
- [ ] Track last-read timestamp per channel (from `conversations.info` / `conversations.mark`)
- [ ] Bold unread channels in sidebar
- [ ] Unread count badge on channels
- [ ] Mark channel as read when viewed (call `conversations.mark`)

### 11.3 Typing Indicators
- [ ] Show "alice is typing..." below message area
- [ ] Auto-clear after 5 seconds
- [ ] Multiple typists: "alice and bob are typing..."

### 11.4 Connection Status
- [ ] Status bar shows connection state: Connected / Reconnecting... / Disconnected
- [ ] Reconnection indicator: subtle banner "Reconnecting..." at top of message area
- [ ] On reconnect: fetch missed messages since last known ts

### 11.5 Build & Verify
- [ ] New messages from other users appear in real-time
- [ ] Reactions update live
- [ ] Presence changes reflected in sidebar
- [ ] Typing indicators work
- [ ] Reconnection works after network interruption

---

## Phase 12: Qt GUI — Thread View

**Goal:** Side panel for viewing and replying to threads.

### 12.1 Thread Panel (`slack/gui/thread-view.ss`)
- [ ] Opens as right-side panel (splitter) when clicking "N replies" on a message
- [ ] Shows parent message at top
- [ ] Lists all replies below in chronological order
- [ ] Own input bar at bottom for replies
- [ ] Close button (X) to dismiss panel
- [ ] "Also send to #channel" checkbox
- [ ] New replies appear live via Socket Mode events
- [ ] Thread panel resizable via splitter

### 12.2 Build & Verify
- [ ] Click thread indicator → panel opens with replies
- [ ] Reply in thread → message appears in thread panel
- [ ] Real-time thread updates work
- [ ] Close panel returns to full-width message area

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
