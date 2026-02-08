;; -*- Gerbil -*-
;; Tests for slack/markdown

(import
  :std/test
  :std/sugar
  :ober/slack/markdown)

(export markdown-test)

(def markdown-test
  (test-suite "slack/markdown"

    ;; --- Plain Text Tests ---

    (test-case "mrkdwn->plain: bold"
      (check (mrkdwn->plain "*bold text*") => "bold text"))

    (test-case "mrkdwn->plain: italic"
      (check (mrkdwn->plain "_italic text_") => "italic text"))

    (test-case "mrkdwn->plain: strikethrough"
      (check (mrkdwn->plain "~struck text~") => "struck text"))

    (test-case "mrkdwn->plain: mixed formatting"
      (check (mrkdwn->plain "Hello *bold* and _italic_ and ~struck~")
             => "Hello bold and italic and struck"))

    (test-case "mrkdwn->plain: inline code preserved"
      (check (mrkdwn->plain "Use `foo()` here") => "Use `foo()` here"))

    (test-case "mrkdwn->plain: code block indented"
      (check (mrkdwn->plain "before\n```\ncode line 1\ncode line 2\n```\nafter")
             => "before\n    \n    code line 1\n    code line 2\nafter"))

    (test-case "mrkdwn->plain: code block preserves formatting chars"
      (check (mrkdwn->plain "```\n*not bold* _not italic_\n```")
             => "    \n    *not bold* _not italic_"))

    (test-case "mrkdwn->plain: blockquote"
      (check (mrkdwn->plain "> quoted text") => "| quoted text"))

    (test-case "mrkdwn->plain: user mention with label"
      (check (mrkdwn->plain "Hey <@U123|alice>!") => "Hey @alice!"))

    (test-case "mrkdwn->plain: user mention without label"
      (check (mrkdwn->plain "Hey <@U123>!") => "Hey @U123!"))

    (test-case "mrkdwn->plain: user mention with resolver"
      (set-user-resolver! (lambda (id) (if (string=? id "U123") "alice" #f)))
      (check (mrkdwn->plain "Hey <@U123>!") => "Hey @alice!")
      (set-user-resolver! #f))

    (test-case "mrkdwn->plain: channel mention with label"
      (check (mrkdwn->plain "See <#C123|general>") => "See #general"))

    (test-case "mrkdwn->plain: channel mention without label"
      (check (mrkdwn->plain "See <#C123>") => "See #C123"))

    (test-case "mrkdwn->plain: channel mention with resolver"
      (set-channel-resolver! (lambda (id) (if (string=? id "C123") "general" #f)))
      (check (mrkdwn->plain "See <#C123>") => "See #general")
      (set-channel-resolver! #f))

    (test-case "mrkdwn->plain: URL with label"
      (check (mrkdwn->plain "Visit <https://example.com|Example>")
             => "Visit Example (https://example.com)"))

    (test-case "mrkdwn->plain: bare URL"
      (check (mrkdwn->plain "Visit <https://example.com>")
             => "Visit https://example.com"))

    (test-case "mrkdwn->plain: emoji shortcodes preserved"
      (check (mrkdwn->plain "Great work :tada: :+1:") => "Great work :tada: :+1:"))

    (test-case "mrkdwn->plain: #f input"
      (check (mrkdwn->plain #f) => ""))

    (test-case "mrkdwn->plain: empty string"
      (check (mrkdwn->plain "") => ""))

    ;; --- HTML Tests ---

    (test-case "mrkdwn->html: bold"
      (check (mrkdwn->html "*bold text*") => "<b>bold text</b>"))

    (test-case "mrkdwn->html: italic"
      (check (mrkdwn->html "_italic text_") => "<i>italic text</i>"))

    (test-case "mrkdwn->html: strikethrough"
      (check (mrkdwn->html "~struck text~") => "<s>struck text</s>"))

    (test-case "mrkdwn->html: inline code"
      (check (mrkdwn->html "Use `foo()` here") => "Use <code>foo()</code> here"))

    (test-case "mrkdwn->html: code block"
      (check (mrkdwn->html "```\ncode\n```")
             => "<pre><code>\ncode\n</code></pre>"))

    (test-case "mrkdwn->html: HTML entities escaped"
      (check (mrkdwn->html "x < y & z > w") => "x &lt; y &amp; z &gt; w"))

    (test-case "mrkdwn->html: code block escapes HTML"
      (check (mrkdwn->html "```\n<script>alert('xss')</script>\n```")
             => "<pre><code>\n&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;\n</code></pre>"))

    (test-case "mrkdwn->html: user mention"
      (check (mrkdwn->html "Hey <@U123|alice>!")
             => "Hey <span class=\"mention\">@alice</span>!"))

    (test-case "mrkdwn->html: channel mention"
      (check (mrkdwn->html "See <#C123|general>")
             => "See <span class=\"channel\">#general</span>"))

    (test-case "mrkdwn->html: URL link"
      (check (mrkdwn->html "Visit <https://example.com|Example>")
             => "Visit <a href=\"https://example.com\">Example</a>"))

    (test-case "mrkdwn->html: newlines become <br>"
      (check (mrkdwn->html "line1\nline2") => "line1<br>\nline2"))

    (test-case "mrkdwn->html: #f input"
      (check (mrkdwn->html #f) => ""))

    ;; --- Edge Cases ---

    (test-case "multiple user mentions"
      (check (mrkdwn->plain "cc <@U111|alice> <@U222|bob>")
             => "cc @alice @bob"))

    (test-case "mixed content"
      (check (mrkdwn->plain "*Hey* <@U123|alice>, check <https://example.com|this link> in <#C456|general>")
             => "Hey @alice, check this link (https://example.com) in #general"))))
