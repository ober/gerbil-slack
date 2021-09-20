#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("slack/client"
    (static-exe:
     "slack/slack"
     "-cc-options"
     "-I/usr/pkg/include"
     "-ld-options"
     "-lpthread -lyaml -lssl -lz -L/usr/local/lib -L/usr/local/Cellar/openssl@1.1/1.1.1l/include/ -L/usr/lib64 -L/usr/pkg/lib")))
