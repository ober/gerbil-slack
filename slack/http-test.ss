;; -*- Gerbil -*-
;; Tests for slack/http

(import
  :std/test
  :std/sugar
  :ober/slack/http)

(export http-test)

(def http-test
  (test-suite "slack/http"

    (test-case "slack-error struct"
      (let ((e (make-slack-error method: "auth.test" code: "invalid_auth" message: "Invalid token")))
        (check (slack-error? e) => #t)
        (check (slack-error-method e) => "auth.test")
        (check (slack-error-code e) => "invalid_auth")
        (check (slack-error-message e) => "Invalid token")))

    (test-case "params->query encodes parameters"
      (check (params->query []) => "")
      (check (params->query '(("channel" . "C123"))) => "channel=C123")
      (check (params->query '(("q" . "hello world"))) => "q=hello%20world"))

    (test-case "params->hash converts alist to hash"
      (let ((h (params->hash '(("a" . "1") ("b" . "2")))))
        (check (hash-ref h "a") => "1")
        (check (hash-ref h "b") => "2")))

    (test-case "url-encode handles special characters"
      (check (url-encode "hello") => "hello")
      (check (url-encode "hello world") => "hello%20world")
      (check (url-encode "a+b=c") => "a%2Bb%3Dc")
      (check (url-encode "foo@bar.com") => "foo%40bar.com"))

    (test-case "string-join works"
      (check (string-join [] "&") => "")
      (check (string-join ["a"] "&") => "a")
      (check (string-join ["a" "b" "c"] "&") => "a&b&c"))

    (test-case "alist-get case-insensitive"
      (let ((hdrs '(("Content-Type" . "application/json")
                    ("Retry-After" . "5"))))
        (check (alist-get hdrs "content-type") => "application/json")
        (check (alist-get hdrs "retry-after") => "5")
        (check (alist-get hdrs "X-Missing") => #f)))))
