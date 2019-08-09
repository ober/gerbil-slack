#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)
(import :std/make)

;;(defbuild-script '("slack.ss"))

(def build-spec
  '("slack"
    (exe: "slack"
	  "-cc-options" "-I/usr/local/include"
	  "-ld-options" "-lyaml -lz -L/usr/local/lib/"
	  )))


(def build-spec-static
  '("slack"
    (static-exe: "slack"
		 "-cc-options" "-I/usr/local/include"
                 "-ld-options" "-lyaml -lz -L/usr/local/lib"
                 "-prelude" "(declare (not safe))")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["deps"]
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["static"]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             static: #t
             depgraph: depgraph
             prefix: "slack"
             build-spec-static)))
    ([]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: 'env
             static: #t
             depgraph: depgraph
             prefix: "slack"
             build-spec)))))
