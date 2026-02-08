;; -*- Gerbil -*-
;; Output formatters for CLI display

(import
  :std/format
  :std/sugar
  :std/text/json
  :ober/slack/types
  :ober/slack/markdown)

(export #t)

;;;; Color Support

(def *color-enabled* #t)

(def (color-enabled?)
  (and *color-enabled*
       (not (getenv "NO_COLOR" #f))))

(def (set-color! enabled)
  (set! *color-enabled* enabled))

(def (ansi code text)
  (if (color-enabled?)
    (string-append "\x1b;[" code "m" text "\x1b;[0m")
    text))

(def (bold text) (ansi "1" text))
(def (dim text) (ansi "2" text))
(def (green text) (ansi "32" text))
(def (yellow text) (ansi "33" text))
(def (cyan text) (ansi "36" text))
(def (red text) (ansi "31" text))
(def (magenta text) (ansi "35" text))

;;;; Timestamp Formatting

(def (format-timestamp ts)
  "Convert Slack timestamp (e.g. '1234567890.123456') to human-readable local time."
  (if (not ts) ""
    (let* ((secs (if (string? ts)
                   (let ((dot (string-index ts #\.)))
                     (if dot
                       (string->number (substring ts 0 dot))
                       (string->number ts)))
                   ts))
           (time (seconds->time secs)))
      (format "~a ~2,'0d:~2,'0d"
              (format-date time)
              (time->hours time)
              (time->minutes time)))))

(def (format-timestamp-short ts)
  "Short time format: HH:MM."
  (if (not ts) ""
    (let* ((secs (if (string? ts)
                   (let ((dot (string-index ts #\.)))
                     (if dot
                       (string->number (substring ts 0 dot))
                       (string->number ts)))
                   ts))
           (time (seconds->time secs)))
      (format "~2,'0d:~2,'0d"
              (time->hours time)
              (time->minutes time)))))

(def (seconds->time secs)
  (if (number? secs)
    (time->utc-components secs)
    #f))

(def (time->utc-components epoch-secs)
  "Convert epoch seconds to (year month day hours minutes seconds)."
  (let* ((s (inexact->exact (floor epoch-secs)))
         ;; Use Gambit's built-in time conversion
         (time-obj (seconds->time-point s))
         (date (time-point->date time-obj)))
    date))

(def (seconds->time-point s)
  (##flonum.<-fixnum s))

(def (time-point->date tp)
  ;; Use Gambit's ##make-date with epoch
  (##date-from-seconds (##flonum->fixnum tp)))

(def (time->hours date)
  (##date-hour date))

(def (time->minutes date)
  (##date-minute date))

(def (format-date date)
  (let ((month (##date-month date))
        (day (##date-day date)))
    (format "~a ~d"
            (vector-ref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                        month)
            day)))

(def (string-index s ch)
  (let loop ((i 0))
    (cond
     ((>= i (string-length s)) #f)
     ((char=? (string-ref s i) ch) i)
     (else (loop (+ i 1))))))

;;;; Text Truncation

(def (truncate-text text max-width)
  "Truncate text with ellipsis if longer than max-width."
  (if (or (not text) (<= (string-length text) max-width))
    (or text "")
    (string-append (substring text 0 (- max-width 3)) "...")))

;;;; Table Formatter

(def (format-table headers rows)
  "Format data as an aligned table with unicode box-drawing.
   headers: list of header strings
   rows: list of lists (each row is a list of strings)"
  (let* ((all-rows (cons headers rows))
         (widths (compute-column-widths all-rows))
         (lines []))
    ;; Top border
    (displayln (table-border "┌" "┬" "┐" widths))
    ;; Header
    (displayln (table-row headers widths #t))
    ;; Header separator
    (displayln (table-border "├" "┼" "┤" widths))
    ;; Data rows
    (for-each (lambda (row) (displayln (table-row row widths #f))) rows)
    ;; Bottom border
    (displayln (table-border "└" "┴" "┘" widths))))

(def (compute-column-widths rows)
  "Compute max width for each column."
  (if (null? rows) []
    (let ((ncols (length (car rows))))
      (let loop ((rows rows)
                 (widths (make-list ncols 0)))
        (if (null? rows) widths
          (loop (cdr rows)
                (map max widths
                     (map (lambda (cell)
                            (string-length (if (string? cell) cell
                                             (if cell (format "~a" cell) ""))))
                          (pad-row (car rows) ncols)))))))))

(def (pad-row row ncols)
  (if (>= (length row) ncols) row
    (append row (make-list (- ncols (length row)) ""))))

(def (table-border left mid right widths)
  (string-append
   left
   (string-join-with "─"
     (map (lambda (w) (make-string (+ w 2) #\─)) widths)
     mid)
   right))

(def (table-row cells widths header?)
  (string-append
   "│"
   (string-join-with ""
     (map (lambda (cell width)
            (let ((s (if (string? cell) cell
                       (if cell (format "~a" cell) ""))))
              (string-append " " (pad-right s width) " │")))
          (pad-row cells (length widths))
          widths)
     "")
   ""))

(def (pad-right s width)
  (let ((len (string-length s)))
    (if (>= len width) s
      (string-append s (make-string (- width len) #\space)))))

(def (string-join-with unused lst sep)
  "Join list of strings with separator."
  (if (null? lst) ""
    (let loop ((rest (cdr lst)) (acc (car lst)))
      (if (null? rest) acc
        (loop (cdr rest) (string-append acc sep (car rest)))))))

;;;; Message Formatter

(def (format-message msg (user-name #f))
  "Format a single message for display."
  (let* ((ts (format-timestamp-short (message-ts msg)))
         (name (or user-name (message-user msg) "unknown"))
         (text (mrkdwn->plain (or (message-text msg) ""))))
    (format "~a ~a: ~a"
            (dim (string-append "[" ts "]"))
            (bold (cyan name))
            text)))

(def (format-messages msgs (user-resolver #f))
  "Format a list of messages for display."
  (for-each
    (lambda (msg)
      (let ((name (if user-resolver
                    (user-resolver (message-user msg))
                    (message-user msg))))
        (displayln (format-message msg name))))
    msgs))

;;;; User Formatter

(def (format-user u)
  "Format a user for display."
  (let ((presence (user-presence u))
        (name (user-display u)))
    (format "~a ~a~a"
            (if (and presence (string=? presence "active"))
              (green "●")
              (dim "○"))
            (bold name)
            (let ((status (user-status-text u)))
              (if (and status (not (string=? status "")))
                (format " — ~a" (dim status))
                "")))))

;;;; Channel Formatter

(def (format-channel ch)
  "Format a channel for display."
  (format "~a ~a~a"
          (if (channel-is-private ch) "🔒" "#")
          (bold (channel-name ch))
          (let ((topic (channel-topic ch)))
            (if (and topic (not (string=? topic "")))
              (format " — ~a" (dim (truncate-text topic 60)))
              ""))))

;;;; JSON Output

(def (output-json data)
  "Output data as formatted JSON."
  (displayln (json-object->string data)))
