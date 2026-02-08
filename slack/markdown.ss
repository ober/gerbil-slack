;; -*- Gerbil -*-
;; Slack mrkdwn → plain text and → HTML conversion

(import
  :std/pregexp
  :std/sugar)

(export
  mrkdwn->plain mrkdwn->html
  set-user-resolver! set-channel-resolver!)

;;;; User/Channel Resolution

(def *user-resolver* #f)
(def *channel-resolver* #f)

(def (set-user-resolver! f) (set! *user-resolver* f))
(def (set-channel-resolver! f) (set! *channel-resolver* f))

(def (resolve-user-id id)
  (if *user-resolver*
    (or (*user-resolver* id) id)
    id))

(def (resolve-channel-id id)
  (if *channel-resolver*
    (or (*channel-resolver* id) id)
    id))

;;;; Plain Text Conversion

(def (mrkdwn->plain text)
  "Convert Slack mrkdwn to plain text for CLI display."
  (if (or (not text) (string=? text "")) ""
    (let* ((parts (extract-code-blocks text))
           (s (car parts))
           (blocks (cadr parts))
           (inlines (caddr parts))
           ;; Resolve mentions, channels, URLs
           (s (resolve-mentions-plain s))
           (s (resolve-channels-plain s))
           (s (resolve-links-plain s))
           ;; Remove formatting markers
           (s (pregexp-replace* "\\*([^*]+)\\*" s "\\1"))
           (s (pregexp-replace* "_([^_]+)_" s "\\1"))
           (s (pregexp-replace* "~([^~]+)~" s "\\1"))
           ;; Blockquotes (line-by-line)
           (s (process-blockquotes-plain s))
           ;; Restore code
           (s (restore-code-blocks-plain s blocks))
           (s (restore-inline-code-plain s inlines)))
      s)))

;;;; HTML Conversion

(def (mrkdwn->html text)
  "Convert Slack mrkdwn to HTML for Qt rich text display."
  (if (or (not text) (string=? text "")) ""
    (let* ((parts (extract-code-blocks text))
           (s (car parts))
           (blocks (cadr parts))
           (inlines (caddr parts))
           ;; HTML-escape first
           (s (html-escape s))
           ;; Resolve mentions, channels, URLs (after escaping)
           (s (resolve-mentions-html s))
           (s (resolve-channels-html s))
           (s (resolve-links-html s))
           ;; Formatting
           (s (pregexp-replace* "\\*([^*]+)\\*" s "<b>\\1</b>"))
           (s (pregexp-replace* "_([^_]+)_" s "<i>\\1</i>"))
           (s (pregexp-replace* "~([^~]+)~" s "<s>\\1</s>"))
           ;; Blockquotes (line-by-line)
           (s (process-blockquotes-html s))
           ;; Newlines → <br>
           (s (pregexp-replace* "\n" s "<br>\n"))
           ;; Restore code
           (s (restore-code-blocks-html s blocks))
           (s (restore-inline-code-html s inlines)))
      s)))

;;;; Code Block Extraction

(def +cb-mark+ "\x1;CB\x1;")
(def +ic-mark+ "\x1;IC\x1;")

(def (extract-code-blocks text)
  "Extract ```code blocks``` and `inline code`, replacing with placeholders.
   Returns (list processed-text code-blocks inline-codes)."
  (let ((code-blocks [])
        (inline-codes [])
        (result text))
    ;; Extract code blocks first (```...```)
    (let loop ()
      (let ((m (pregexp-match-positions "```([\\s\\S]*?)```" result)))
        (when m
          (let* ((full-match (car m))
                 (content-match (cadr m))
                 (idx (length code-blocks))
                 (content (substring result (car content-match) (cdr content-match)))
                 (before (substring result 0 (car full-match)))
                 (after (substring result (cdr full-match) (string-length result))))
            (set! code-blocks (append code-blocks (list content)))
            (set! result (string-append before
                           +cb-mark+ (number->string idx) +cb-mark+
                           after))
            (loop)))))
    ;; Extract inline code (`...`)
    (let loop ()
      (let ((m (pregexp-match-positions "`([^`]+)`" result)))
        (when m
          (let* ((full-match (car m))
                 (content-match (cadr m))
                 (idx (length inline-codes))
                 (content (substring result (car content-match) (cdr content-match)))
                 (before (substring result 0 (car full-match)))
                 (after (substring result (cdr full-match) (string-length result))))
            (set! inline-codes (append inline-codes (list content)))
            (set! result (string-append before
                           +ic-mark+ (number->string idx) +ic-mark+
                           after))
            (loop)))))
    (list result code-blocks inline-codes)))

;;;; Mention/Channel/Link Resolution (Plain Text)

(def (resolve-mentions-plain s)
  (let loop ((text s))
    (let ((m (pregexp-match "<@(U[A-Z0-9]+)\\|([^>]+)>" text)))
      (if m
        ;; Has label
        (loop (string-sub text (car m) (string-append "@" (caddr m))))
        ;; Try without label
        (let ((m2 (pregexp-match "<@(U[A-Z0-9]+)>" text)))
          (if m2
            (loop (string-sub text (car m2)
                    (string-append "@" (resolve-user-id (cadr m2)))))
            text))))))

(def (resolve-channels-plain s)
  (let loop ((text s))
    (let ((m (pregexp-match "<#(C[A-Z0-9]+)\\|([^>]+)>" text)))
      (if m
        (loop (string-sub text (car m) (string-append "#" (caddr m))))
        (let ((m2 (pregexp-match "<#(C[A-Z0-9]+)>" text)))
          (if m2
            (loop (string-sub text (car m2)
                    (string-append "#" (resolve-channel-id (cadr m2)))))
            text))))))

(def (resolve-links-plain s)
  (let loop ((text s))
    (let ((m (pregexp-match "<(https?://[^|>]+)\\|([^>]+)>" text)))
      (if m
        (loop (string-sub text (car m)
                (string-append (caddr m) " (" (cadr m) ")")))
        ;; Bare URLs
        (let ((m2 (pregexp-match "<(https?://[^>]+)>" text)))
          (if m2
            (loop (string-sub text (car m2) (cadr m2)))
            text))))))

;;;; Mention/Channel/Link Resolution (HTML — after html-escape)

(def (resolve-mentions-html s)
  (let loop ((text s))
    (let ((m (pregexp-match "&lt;@(U[A-Z0-9]+)\\|([^&]+)&gt;" text)))
      (if m
        (loop (string-sub text (car m)
                (string-append "<span class=\"mention\">@" (caddr m) "</span>")))
        (let ((m2 (pregexp-match "&lt;@(U[A-Z0-9]+)&gt;" text)))
          (if m2
            (loop (string-sub text (car m2)
                    (string-append "<span class=\"mention\">@"
                                   (resolve-user-id (cadr m2)) "</span>")))
            text))))))

(def (resolve-channels-html s)
  (let loop ((text s))
    (let ((m (pregexp-match "&lt;#(C[A-Z0-9]+)\\|([^&]+)&gt;" text)))
      (if m
        (loop (string-sub text (car m)
                (string-append "<span class=\"channel\">#" (caddr m) "</span>")))
        (let ((m2 (pregexp-match "&lt;#(C[A-Z0-9]+)&gt;" text)))
          (if m2
            (loop (string-sub text (car m2)
                    (string-append "<span class=\"channel\">#"
                                   (resolve-channel-id (cadr m2)) "</span>")))
            text))))))

(def (resolve-links-html s)
  (let loop ((text s))
    (let ((m (pregexp-match "&lt;(https?://[^|&]+)\\|([^&]+)&gt;" text)))
      (if m
        (loop (string-sub text (car m)
                (string-append "<a href=\"" (cadr m) "\">" (caddr m) "</a>")))
        (let ((m2 (pregexp-match "&lt;(https?://[^&]+)&gt;" text)))
          (if m2
            (loop (string-sub text (car m2)
                    (string-append "<a href=\"" (cadr m2) "\">" (cadr m2) "</a>")))
            text))))))

;;;; Blockquote Processing (line-by-line, no (?m) flag)

(def (process-blockquotes-plain s)
  (let ((lines (string-split s #\newline)))
    (string-join
     (map (lambda (line)
            (if (and (> (string-length line) 0)
                     (char=? (string-ref line 0) #\>))
              (let ((rest (substring line 1 (string-length line))))
                (string-append "| " (if (and (> (string-length rest) 0)
                                             (char=? (string-ref rest 0) #\space))
                                      (substring rest 1 (string-length rest))
                                      rest)))
              line))
          lines)
     "\n")))

(def (process-blockquotes-html s)
  (let ((lines (string-split s #\newline)))
    (string-join
     (map (lambda (line)
            (cond
             ;; After html-escape, > becomes &gt;
             ((string-prefix? "&gt; " line)
              (string-append "<blockquote>"
                             (substring line 5 (string-length line))
                             "</blockquote>"))
             ((string-prefix? "&gt;" line)
              (string-append "<blockquote>"
                             (substring line 4 (string-length line))
                             "</blockquote>"))
             (else line)))
          lines)
     "\n")))

(def (string-prefix? prefix s)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))

;;;; Code Restoration

(def (restore-code-blocks-plain s blocks)
  (let loop ((text s) (i 0))
    (if (>= i (length blocks))
      text
      (let ((placeholder (string-append +cb-mark+ (number->string i) +cb-mark+))
            (code (list-ref blocks i)))
        (loop (string-sub text placeholder (indent-code code))
              (+ i 1))))))

(def (restore-inline-code-plain s inlines)
  (let loop ((text s) (i 0))
    (if (>= i (length inlines))
      text
      (let ((placeholder (string-append +ic-mark+ (number->string i) +ic-mark+))
            (code (list-ref inlines i)))
        (loop (string-sub text placeholder (string-append "`" code "`"))
              (+ i 1))))))

(def (restore-code-blocks-html s blocks)
  (let loop ((text s) (i 0))
    (if (>= i (length blocks))
      text
      (let ((placeholder (string-append +cb-mark+ (number->string i) +cb-mark+))
            (code (html-escape (list-ref blocks i))))
        (loop (string-sub text placeholder
                (string-append "<pre><code>" code "</code></pre>"))
              (+ i 1))))))

(def (restore-inline-code-html s inlines)
  (let loop ((text s) (i 0))
    (if (>= i (length inlines))
      text
      (let ((placeholder (string-append +ic-mark+ (number->string i) +ic-mark+))
            (code (html-escape (list-ref inlines i))))
        (loop (string-sub text placeholder
                (string-append "<code>" code "</code>"))
              (+ i 1))))))

;;;; Helpers

(def (html-escape s)
  (let* ((s (pregexp-replace* "&" s "&amp;"))
         (s (pregexp-replace* "<" s "&lt;"))
         (s (pregexp-replace* ">" s "&gt;"))
         (s (pregexp-replace* "\"" s "&quot;"))
         (s (pregexp-replace* "'" s "&#x27;")))
    s))

(def (indent-code text)
  (let ((lines (string-split text #\newline)))
    (string-join
     (map (lambda (line) (string-append "    " line)) lines)
     "\n")))

(def (string-sub s old new)
  "Replace first occurrence of old in s with new."
  (let ((pos (string-contains s old)))
    (if pos
      (string-append (substring s 0 pos)
                     new
                     (substring s (+ pos (string-length old))
                                (string-length s)))
      s)))

(def (string-contains s sub)
  (let ((slen (string-length s))
        (sublen (string-length sub)))
    (if (> sublen slen) #f
      (let loop ((i 0))
        (cond
         ((> (+ i sublen) slen) #f)
         ((string=? (substring s i (+ i sublen)) sub) i)
         (else (loop (+ i 1))))))))

(def (string-join lst sep)
  (if (null? lst) ""
    (let loop ((rest (cdr lst)) (acc (car lst)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))
