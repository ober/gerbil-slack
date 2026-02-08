#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("slack/types"
    "slack/config"
    "slack/http"
    "slack/api/auth"
    "slack/api/users"
    "slack/api/conversations"
    "slack/api/chat"
    "slack/api/reactions"
    "slack/api/files"
    "slack/api/pins"
    "slack/api/search"
    "slack/api/emoji"
    "slack/api/team"
    "slack/api/reminders"
    "slack/api/dnd"
    "slack/api/usergroups"
    "slack/api/bookmarks"
    "slack/events"
    "slack/socket"
    "slack/cache"
    "slack/markdown"
    "slack/cli/format"
    "slack/cli/commands"
    "slack/client"
    (exe: "slack/slack")
    "slack/gui/theme"
    "slack/gui/app"
    "slack/gui/sidebar"))
