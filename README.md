# nnreddit

This package implements a Gnus Reddit backend in Emacs Lisp. Each submission and
each comment gets its own stable Gnus article number and Message-ID. Gnus keeps
the read marks. `gnus-thread-reader` is an optional, backend-independent view
over those articles.

Articles use Reddit's rendered HTML when available, without a Markdown MIME
alternative. Gnus and `gnus-thread-reader` display bold text and inline
images; direct image posts and galleries are included. Articles without HTML
use plain text as a fallback.

The backend supports three kinds of groups:

- `subreddit.NAME`: recent submissions in one subreddit. Open a submission to
  fetch its comments.
- `post.ID`: one submission and its reply tree.
- `notifications`: replies addressed to the signed-in account. Messages about
  the same submission share a Gnus thread.

It can submit a text post to a subreddit and reply to a cached submission or
comment from Message mode. A send with an uncertain outcome is locked locally
until you check Reddit, to avoid accidental duplicate posts.

## Requirements

- Emacs 31.1 or newer.
- Reddit Data API access permitted for the installed OAuth app used here. The
  default public client ID came from an earlier nnreddit setup; its current API
  access status has not been verified. You can configure your own approved
  installed-app client ID and matching redirect URI instead.
- A Reddit account to authorize in a browser.

There is no Python, PRAW, browser-cookie, or native-module dependency.

## Setup

```elisp
(add-to-list 'load-path "/path/to/nnreddit/lisp")
(require 'nnreddit)
(setq nnreddit-username "YOUR_REDDIT_USERNAME")
(add-to-list 'gnus-secondary-select-methods '(nnreddit "reddit"))
```

Run `M-x nnreddit-authorize`. Sign in and approve in the browser, then paste
the browser's complete redirect URL into Emacs. The localhost page may say it
cannot connect; its address still contains the one-time authorization code.
The backend checks OAuth state before exchanging that code. It saves the
refresh token in `nnreddit-refresh-token-file` (mode 0600); access tokens stay
in Emacs memory. It never asks Emacs for your Reddit password.
The backend generates its User-Agent from `nnreddit-username`.

The default refresh-token path is under `gnus-directory/nnreddit/`. You can
move the token to an agenix-managed file and point
`nnreddit-refresh-token-file` to its decrypted path. To authorize a new
account, first point the variable to a writable regular file. Metadata and
article bodies are cached under `gnus-directory/nnreddit/`.

Start Gnus, then call `M-x nnreddit-subscribe-subreddit`,
`M-x nnreddit-subscribe-post`, or `M-x nnreddit-subscribe-notifications`.
Inside a Reddit summary, `G` refreshes the group. `RET` fetches the complete
reply tree and opens `gnus-thread-reader` if that separate package is installed.
Normal Gnus article navigation still works without it.

## Development

```sh
emacs -Q --batch -L lisp -f batch-byte-compile lisp/nnreddit.el
emacs -Q --batch -L lisp -L tests -l nnreddit-test -f ert-run-tests-batch-and-exit
```

The tests use local JSON-shaped fixtures and do not contact Reddit or submit
content. Live authentication, API access, and posting have not been exercised
by the offline suite; the default client ID does not prove API approval.
