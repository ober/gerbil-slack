#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("slack/client"
    (exe:
     "slack/slack")))
