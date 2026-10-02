;;; nnreddit.el --- Reddit subscriptions as Gnus articles -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: news, comm

;;; Commentary:
;; Subreddit, single-post and reply-notification subscriptions.  A Reddit post
;; and every comment have separate Gnus article numbers and Message-IDs.
;; gnus-thread-reader is only a view of these ordinary articles.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'dom)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'gnus-start)
(require 'gnus-art)
(require 'nnoo)
(require 'nnheader)
(require 'message)
(require 'mm-decode)
(require 'mail-parse)
(require 'rfc2047)
(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'subr-x)
(declare-function gnus-thread-reader-open "gnus-thread-reader" ())

(defgroup nnreddit nil "Reddit subscriptions in Gnus." :group 'gnus)
(nnoo-declare nnreddit)
(defvoo nnreddit-directory (expand-file-name "nnreddit/" gnus-directory)
  "Directory containing local article metadata and Gnus number mappings.")
(defvoo nnreddit--store nil)
(defvoo nnreddit-status-string "")
(nnoo-define-basics nnreddit)

(defcustom nnreddit-client-id "5oagOpX2_NKVDej_iuZjFA"
  "Public installed-app client ID from an earlier nnreddit setup.
This ID is kept for existing users; it is not an access token or proof that
Reddit currently permits this app's API usage."
  :type 'string :group 'nnreddit)
(defcustom nnreddit-redirect-uri "http://127.0.0.1:17973"
  "OAuth redirect URI for the default installed app."
  :type 'string :group 'nnreddit)
(defcustom nnreddit-refresh-token-file
  (expand-file-name "refresh-token" nnreddit-directory)
  "File holding this account's OAuth refresh token.
Store it outside Git.  An agenix-provided file may be used after the first
authorization, but `nnreddit-authorize' needs a writable path to save it."
  :type 'file :group 'nnreddit)
(defcustom nnreddit-username nil
  "Reddit username used as contact information in API User-Agent headers."
  :type '(choice (const :tag "Unset" nil) string) :group 'nnreddit)
(defvar nnreddit--server "reddit")
(defvar nnreddit--stores (make-hash-table :test #'equal))
(defvar nnreddit--access-token nil)
(defvar nnreddit--access-token-expiry 0)
(defvar nnreddit--last-request-time 0)
(define-error 'nnreddit-incomplete-response "Incomplete Reddit response")
(defcustom nnreddit-request-interval 1
  "Minimum seconds between Reddit Data API requests."
  :type 'number :group 'nnreddit)
(cl-defstruct nnreddit--db file groups sends)

(defun nnreddit--user-agent ()
  "Build a descriptive Reddit User-Agent from `nnreddit-username'."
  (unless (and (stringp nnreddit-username)
               (string-match-p "\\`[A-Za-z0-9_-]+\\'" nnreddit-username))
    (user-error "Set nnreddit-username to your Reddit username"))
  (format "%s:nnreddit-emacs:0.1.0 (by /u/%s)"
          (pcase system-type
            ('gnu/linux "linux") ('darwin "macos") ('windows-nt "windows")
            (_ (symbol-name system-type)))
          nnreddit-username))

(defun nnreddit--oauth-post (fields)
  "Exchange OAuth FIELDS for a token response using the installed app ID."
  (unless (and (stringp nnreddit-client-id)
               (string-match-p "\\`[A-Za-z0-9_-]+\\'" nnreddit-client-id))
    (error "Set a valid Reddit client ID"))
  (let* ((user-agent (nnreddit--user-agent))
         (url-request-method "POST")
         (url-max-redirections 0)
         (url-request-data (url-build-query-string fields))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic "
                                      (base64-encode-string
                                       (concat nnreddit-client-id ":") t)))
            ("User-Agent" . ,user-agent)
            ("Content-Type" . "application/x-www-form-urlencoded")))
         (response (url-retrieve-synchronously
                    "https://www.reddit.com/api/v1/access_token" t t 30)))
    (unless response (error "Reddit OAuth token request timed out"))
    (with-current-buffer response
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (looking-at "HTTP/[0-9.]+ 200")
              (error "Reddit OAuth rejected this app or authorization"))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Malformed Reddit OAuth response"))
            (let ((payload (json-parse-string
                            (buffer-substring-no-properties (point) (point-max))
                            :object-type 'plist)))
              (unless (and (stringp (plist-get payload :access_token))
                           (numberp (plist-get payload :expires_in)))
                (error "Reddit OAuth did not return an access token"))
              payload))
        (kill-buffer response)))))

(defun nnreddit--remember-access-token (payload)
  "Cache the short-lived access token in PAYLOAD."
  (setq nnreddit--access-token (plist-get payload :access_token)
        nnreddit--access-token-expiry
        (+ (float-time) (plist-get payload :expires_in)))
  nnreddit--access-token)

(defun nnreddit--token ()
  "Return an access token, refreshing it with the saved OAuth grant."
  (if (and nnreddit--access-token
           (> nnreddit--access-token-expiry (+ (float-time) 60)))
      nnreddit--access-token
    (unless (and (stringp nnreddit-refresh-token-file)
                 (file-readable-p nnreddit-refresh-token-file))
      (user-error "Run M-x nnreddit-authorize to connect your Reddit account"))
    (let ((refresh (string-trim
                    (with-temp-buffer
                      (insert-file-contents nnreddit-refresh-token-file)
                      (buffer-string)))))
      (unless (and (not (string-empty-p refresh))
                   (not (string-match-p "[[:space:]]" refresh)))
        (error "Invalid Reddit refresh token file"))
      (nnreddit--remember-access-token
       (nnreddit--oauth-post
        `(("grant_type" "refresh_token") ("refresh_token" ,refresh)))))))

(defun nnreddit--authorization-url (state)
  "Return the app's browser authorization URL for STATE."
  (concat "https://www.reddit.com/api/v1/authorize?"
          (url-build-query-string
           `(("client_id" ,nnreddit-client-id) ("response_type" "code")
             ("state" ,state) ("redirect_uri" ,nnreddit-redirect-uri)
             ("duration" "permanent")
             ("scope" "identity read privatemessages submit")))))

(defun nnreddit--complete-authorization (callback state)
  "Exchange CALLBACK after checking STATE, then save the refresh token."
  (let* ((prefix (and (stringp callback)
                      (cl-find-if (lambda (candidate)
                                    (string-prefix-p candidate callback))
                                  (list (concat nnreddit-redirect-uri "?")
                                        (concat nnreddit-redirect-uri "/?"))))))
    (unless prefix
      (error "Paste the complete Reddit redirect URL"))
    (let* ((query (url-parse-query-string
                   (car (split-string (substring callback (length prefix)) "#"))))
         (returned-state (cadr (assoc "state" query)))
         (code (cadr (assoc "code" query))))
    (unless (equal state returned-state)
      (error "Reddit OAuth state does not match"))
    (unless (and (stringp code) (not (string-empty-p code)))
      (error "Reddit authorization was declined or no code was returned"))
    (let* ((payload (nnreddit--oauth-post
                     `(("grant_type" "authorization_code")
                       ("code" ,code) ("redirect_uri" ,nnreddit-redirect-uri))))
           (refresh (plist-get payload :refresh_token))
           (file nnreddit-refresh-token-file)
           (directory (file-name-directory file)) temporary)
      (unless (and (stringp refresh) (not (string-empty-p refresh)))
        (error "Reddit did not return a permanent refresh token"))
      (when (file-symlink-p file)
        (error "Refresh-token path is a symlink; use a writable regular file"))
      (make-directory directory t)
      (unwind-protect
          (progn
            (setq temporary (make-temp-file (expand-file-name ".nnreddit-token-" directory)))
            (set-file-modes temporary #o600)
            (with-temp-file temporary (insert refresh "\n"))
            (rename-file temporary file t)
            (nnreddit--remember-access-token payload))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary)))))))

;;;###autoload
(defun nnreddit-authorize ()
  "Authorize Reddit in a browser, then paste its final redirect URL.
The localhost page may fail to load; copy its complete URL from the browser."
  (interactive)
  (nnreddit--user-agent)
  (let* ((state (secure-hash
                 'sha256 (format "%s:%s:%s" (float-time) (random) (emacs-pid))))
         (url (nnreddit--authorization-url state)))
    (browse-url url)
    (nnreddit--complete-authorization
     (read-string "Paste the complete Reddit redirect URL: ") state)
    (message "Reddit authorization saved")))

(defun nnreddit--request (method path fields)
  "Call Reddit Data API PATH using METHOD and FIELDS; return parsed JSON."
  (unless (and (stringp path) (string-prefix-p "/" path)
               (not (string-match-p "[?#\r\n]" path)))
    (error "Invalid Reddit API path"))
  (let* ((token (nnreddit--token))
         (user-agent (nnreddit--user-agent))
         (url-request-method (if (eq method 'post) "POST" "GET"))
         (url-max-redirections 0)
         (params (append fields '(("raw_json" "1") ("api_type" "json"))))
         (url-request-data (when (eq method 'post) (url-build-query-string params)))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " token))
            ("User-Agent" . ,user-agent)
            ("Content-Type" . "application/x-www-form-urlencoded")))
         (url (concat "https://oauth.reddit.com" path
                      (unless (eq method 'post)
                        (concat "?" (url-build-query-string params))))))
    ;; A dropped TLS connection can leave a 200 response with an empty or
    ;; truncated body.  Retry reads, but never repeat a write after an
    ;; ambiguous failure.
    (catch 'nnreddit-response
      (dotimes (attempt (if (eq method 'post) 1 2))
        (let ((delay (- nnreddit-request-interval
                        (- (float-time) nnreddit--last-request-time))))
          (when (> delay 0) (sleep-for delay)))
        (setq nnreddit--last-request-time (float-time))
        (condition-case problem
            (let ((response
                   (condition-case err
                       (url-retrieve-synchronously url t t 30)
                     (gnutls-error
                      (signal 'nnreddit-incomplete-response
                              (list (error-message-string err)))))))
              (unless response
                (signal 'nnreddit-incomplete-response '("request timed out")))
              (with-current-buffer response
                (unwind-protect
                    (progn
                      (goto-char (point-min))
                      (unless (looking-at "HTTP/[0-9.]+ 2[0-9][0-9]")
                        (error "Reddit API request failed: %s"
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))
                      (unless (re-search-forward "\r?\n\r?\n" nil t)
                        (signal 'nnreddit-incomplete-response
                                '("missing HTTP body")))
                      (let* ((body (buffer-substring-no-properties
                                    (point) (point-max)))
                             (payload
                              (condition-case nil
                                  (json-parse-string
                                   body :object-type 'plist :array-type 'list
                                   :null-object nil :false-object nil)
                                (error
                                 (signal 'nnreddit-incomplete-response
                                         '("invalid or truncated JSON")))))
                             (errors (plist-get (plist-get payload :json) :errors)))
                        (when errors
                          (error "Reddit API rejected request: %S" errors))
                        (throw 'nnreddit-response payload)))
                  (kill-buffer response))))
          (nnreddit-incomplete-response
           (when (or (eq method 'post) (= attempt 1))
             (error "Reddit API response interrupted (%s); try again later"
                    (error-message-string problem)))))))))

(defun nnreddit--listing (payload)
  "Return the children of Reddit Listing PAYLOAD."
  (plist-get (plist-get payload :data) :children))

(defun nnreddit--things (payload)
  "Return API action result things from PAYLOAD."
  (plist-get (plist-get (plist-get payload :json) :data) :things))

(defun nnreddit--image-url-p (url)
  "Return non-nil when URL names an HTTPS image."
  (and (stringp url)
       (string-match-p "\\`https://[^[:space:]\"<>]+\\'" url)
       (let ((parsed (url-generic-parse-url url)))
         (and (equal (url-type parsed) "https")
              (url-host parsed)
              (string-match-p
               "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\)\\(?:[?#]\\|\\'\\)"
               (url-filename parsed))))))

(defun nnreddit--post-images (data)
  "Return direct image URLs for gallery or image-post DATA."
  (let ((metadata (plist-get data :media_metadata))
        (items (plist-get (plist-get data :gallery_data) :items)))
    (if items
        (cl-loop for item in items
                 for id = (plist-get item :media_id)
                 for media = (and (stringp id)
                                  (plist-get metadata (intern (concat ":" id))))
                 for url = (plist-get (plist-get media :s) :u)
                 when (nnreddit--image-url-p url)
                 collect (replace-regexp-in-string "&amp;" "&" url t t))
      (let ((url (plist-get data :url_overridden_by_dest)))
        (when (nnreddit--image-url-p url) (list url))))))

(defun nnreddit--article-html (entry)
  "Return Reddit-rendered HTML for ENTRY, including inline images."
  (let ((html (plist-get entry :html))
        (images (plist-get entry :images)))
    (when (or (and (stringp html) (not (string-empty-p html))) images)
      (with-temp-buffer
        (insert (if (and (stringp html) (not (string-empty-p html)))
                    html
                  "<html><body></body></html>"))
        (let ((document (libxml-parse-html-region (point-min) (point-max))))
          (dolist (anchor (dom-by-tag document 'a))
            (let ((href (dom-attr anchor 'href)))
              (when (and (nnreddit--image-url-p href)
                         (equal (string-trim (dom-inner-text anchor)) href))
                (setcdr (cdr anchor)
                        (list `(img ((src . ,href) (alt . "Reddit image"))))))))
          (when-let* ((body (car (dom-by-tag document 'body))))
            (setcdr (cdr body)
                    (append (dom-children body)
                            (mapcar (lambda (url)
                                      `(p nil (img ((src . ,url)
                                                    (alt . "Reddit image")))))
                                    images))))
          (erase-buffer)
          (dom-print document)
          (buffer-string))))))

(defun nnreddit--fullname (id kind)
  "Check Reddit full name ID starts with KIND."
  (unless (and (stringp id)
               (string-match-p (format "\\`%s_[a-z0-9]+\\'" kind) id))
    (error "Unexpected Reddit %s ID" kind))
  id)

(defun nnreddit--post-record (thing)
  "Normalize a Reddit submission THING into a local article record."
  (let* ((data (plist-get thing :data))
         (id (plist-get data :name))
         (body (or (plist-get data :selftext) "")))
    (nnreddit--fullname id "t3")
    (list :id id :parent nil :root id :title (plist-get data :title)
          :author (or (plist-get data :author) "[deleted]")
          :body (if (member body '("[removed]" "[deleted]")) "" body)
          :html (unless (member body '("[removed]" "[deleted]"))
                  (plist-get data :selftext_html))
          :images (nnreddit--post-images data)
          :created (plist-get data :created_utc)
          :url (concat "https://www.reddit.com" (or (plist-get data :permalink) ""))
          :subreddit (plist-get data :subreddit)
          :deleted (and (member (plist-get data :author) '("[deleted]" nil)) t))))

(defun nnreddit--comment-record (thing root title subreddit)
  "Normalize comment THING in ROOT into a Gnus article record."
  (let* ((data (plist-get thing :data))
         (id (nnreddit--fullname (plist-get data :name) "t1"))
         (parent (plist-get data :parent_id))
         (body (or (plist-get data :body) "")))
    (unless (and (stringp parent)
                 (member (substring parent 0 2) '("t1" "t3"))
                 (equal (plist-get data :link_id) root))
      (error "Reddit returned a comment outside the requested thread"))
    (list :id id :parent parent :root root :title title
          :author (or (plist-get data :author) "[deleted]")
          :body (if (member body '("[removed]" "[deleted]")) "" body)
          :html (unless (member body '("[removed]" "[deleted]"))
                  (plist-get data :body_html))
          :created (plist-get data :created_utc)
          :url (concat "https://www.reddit.com"
                       (or (plist-get data :permalink) ""))
          :subreddit subreddit
          :deleted (and (member body '("[removed]" "[deleted]")) t))))

(defun nnreddit--thread (post-id)
  "Fetch POST-ID and its comments as independent Gnus article records."
  (unless (string-match-p "\\`[a-z0-9]+\\'" post-id)
    (error "Invalid Reddit post ID"))
  (let* ((payload (nnreddit--request
                   'get (format "/comments/%s.json" post-id)
                   '(("limit" "100") ("sort" "old"))))
         (post (car (nnreddit--listing (car payload))))
         (data (plist-get post :data))
         (root (nnreddit--fullname (plist-get data :name) "t3"))
         (title (plist-get data :title))
         (subreddit (plist-get data :subreddit))
         (known (make-hash-table :test #'equal))
         (queue (nnreddit--listing (cadr payload)))
         (expanded (make-hash-table :test #'equal))
         (steps 0))
    (unless (equal root (concat "t3_" post-id))
      (error "Reddit returned a different post"))
    (puthash root (nnreddit--post-record post) known)
    (while queue
      (when (> (cl-incf steps) 10000)
        (error "Reddit comment pagination exceeded its safety limit"))
      (let* ((thing (pop queue))
             (kind (plist-get thing :kind))
             (item (plist-get thing :data)))
        (pcase kind
          ("t1"
           (let ((record (nnreddit--comment-record thing root title subreddit))
                 (replies (plist-get item :replies)))
             (puthash (plist-get record :id) record known)
             (when (and (listp replies) (equal (plist-get replies :kind) "Listing"))
               (setq queue (append (nnreddit--listing replies) queue)))))
          ("more"
           (let ((children (plist-get item :children)))
             (if children
                 (while children
                   (let* ((batch (cl-loop repeat 100 while children collect (pop children)))
                          (response (nnreddit--request
                                     'get "/api/morechildren.json"
                                     `(("link_id" ,root)
                                       ("children" ,(string-join batch ","))
                                       ("sort" "old")))))
                     (setq queue (append (nnreddit--things response) queue))))
               (let ((parent (plist-get item :parent_id)))
                 (when (and (stringp parent)
                            (string-prefix-p "t1_" parent)
                            (not (gethash parent expanded)))
                   (puthash parent t expanded)
                   (let ((page (nnreddit--request
                                'get (format "/comments/%s.json" post-id)
                                `(("comment" ,(substring parent 3))
                                  ("context" "0") ("limit" "100")))))
                     (setq queue (append (nnreddit--listing (cadr page)) queue))))))))
          (_ (error "Unexpected Reddit comment kind: %S" kind)))))
    (cl-loop for article being the hash-values of known collect article)))

(defun nnreddit--operation (request)
  "Perform normalized Reddit REQUEST in Emacs Lisp."
  (pcase (plist-get request :action)
    ("subreddit"
     (let* ((name (plist-get request :subreddit))
            (payload (nnreddit--request 'get (format "/r/%s/new.json" name)
                                       '(("limit" "25")))))
       (list :posts (mapcar #'nnreddit--post-record
                            (nnreddit--listing payload)))))
    ("thread"
     (list :articles (nnreddit--thread (plist-get request :post_id))))
    ("notifications"
     (let* ((payload (nnreddit--request 'get "/message/inbox.json"
                                       '(("limit" "100") ("mark" "false"))))
            (targets (make-hash-table :test #'equal)) threads)
       (dolist (thing (nnreddit--listing payload))
         (let* ((data (plist-get thing :data))
                (id (plist-get data :name))
                (root (plist-get data :link_id)))
           (when (and (stringp id) (string-match-p "\\`t1_[a-z0-9]+\\'" id)
                      (stringp root) (string-match-p "\\`t3_[a-z0-9]+\\'" root))
             (push id (gethash root targets)))))
       (maphash (lambda (root ids)
                  (push (list :post_id (substring root 3) :targets ids
                              :articles (nnreddit--thread (substring root 3))) threads))
                targets)
       (list :threads threads)))
    ("reply"
     (let* ((parent (plist-get request :parent))
            (payload (nnreddit--request 'post "/api/comment.json"
                                       `(("thing_id" ,parent)
                                         ("text" ,(plist-get request :body)))))
            (things (nnreddit--things payload))
            (thing (car things))
            (data (plist-get thing :data)))
       (unless (and (= (length things) 1) (equal (plist-get data :parent_id) parent))
         (error "Reddit did not confirm the new reply; check the website"))
       (list :article
             (list :id (plist-get data :name) :parent parent
                   :root (plist-get data :link_id) :title nil
                   :author (plist-get data :author) :body (plist-get data :body)
                   :created (plist-get data :created_utc)
                   :url (concat "https://www.reddit.com"
                                (or (plist-get data :permalink) ""))))))
    ("submit"
     (let* ((name (plist-get request :subreddit))
            (payload (nnreddit--request
                      'post "/api/submit.json"
                      `(("sr" ,name) ("kind" "self")
                        ("title" ,(plist-get request :title))
                        ("text" ,(plist-get request :body)))))
            (data (plist-get (plist-get payload :json) :data))
            (id (plist-get data :name)))
       (nnreddit--message-id id)
       (list :article
             (list :id id :parent nil :root id :title (plist-get request :title)
                   :author "me"
                   :body (plist-get request :body) :created (float-time)
                   :url (plist-get data :url) :subreddit name))))
    (_ (error "Unsupported Reddit operation"))))

(defun nnreddit--load (file)
  "Load the local FILE without evaluating code."
  (let ((store (make-nnreddit--db :file file)))
    (when (file-readable-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))
        (unless (equal (plist-get data :version) 1)
          (error "Unsupported nnreddit cache version"))
        (setf (nnreddit--db-groups store) (plist-get data :groups)
              (nnreddit--db-sends store) (plist-get data :sends))))
    store))

(defun nnreddit--save (store)
  "Atomically save STORE with mode 0600."
  (let* ((file (nnreddit--db-file store))
         (directory (file-name-directory file)) temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".nnreddit-" directory)))
          (set-file-modes temporary #o600)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary
              (insert (json-serialize
                       (list :version 1
                             :groups (vconcat
                                      (mapcar (lambda (group)
                                                (let ((copy (copy-sequence group)))
                                                  (setf (plist-get copy :entries)
                                                        (vconcat
                                                         (mapcar
                                                          (lambda (entry)
                                                            (let ((item (copy-sequence entry)))
                                                              (when (plist-get item :images)
                                                                (setf (plist-get item :images)
                                                                      (vconcat (plist-get item :images))))
                                                              item))
                                                          (plist-get group :entries))))
                                                  copy))
                                              (nnreddit--db-groups store)))
                             :sends (vconcat (nnreddit--db-sends store)))
                       :null-object nil :false-object :false))))
          (rename-file temporary file t))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(deffoo nnreddit-open-server (server &optional defs _connectionless)
  (condition-case problem
      (progn
        (nnoo-change-server 'nnreddit server defs)
        (let ((file (expand-file-name (concat (secure-hash 'sha256 server) ".json")
                                      nnreddit-directory)))
          (setq nnreddit--store (or (gethash file nnreddit--stores)
                                    (puthash file (nnreddit--load file) nnreddit--stores))))
        t)
    (error (nnheader-report 'nnreddit "%s" (error-message-string problem)))))

(defun nnreddit--select (&optional server)
  "Select SERVER's store."
  (when server
    (unless (nnreddit-open-server server) (error "%s" nnreddit-status-string)))
  (unless nnreddit--store (error "No open nnreddit server"))
  nnreddit--store)

(defun nnreddit--group (store name)
  "Find NAME in STORE."
  (cl-find name (nnreddit--db-groups store) :test #'equal
           :key (lambda (group) (plist-get group :name))))

(defun nnreddit--ensure-group (store name)
  "Find or create NAME, validating its subscription shape."
  (or (nnreddit--group store name)
      (let ((kind (cond ((string-match-p "\\`subreddit\\.[A-Za-z0-9_]+\\'" name) "subreddit")
                        ((string-match-p "\\`post\\.[a-z0-9]+\\'" name) "post")
                        ((equal name "notifications") "notifications"))))
        (unless kind (error "Use subreddit.NAME, post.ID or notifications"))
        (let ((group (list :name name :kind kind :next 1 :entries nil)))
          (push group (nnreddit--db-groups store))
          (nnreddit--save store)
          group))))

(defun nnreddit--entry (group id)
  "Find ID, a local number or Reddit fullname, in GROUP."
  (cl-find-if (lambda (entry)
                (if (numberp id) (= id (plist-get entry :number))
                  (equal id (plist-get entry :id))))
              (plist-get group :entries)))

(defun nnreddit--message-id (id)
  "Return a stable Gnus Message-ID for Reddit fullname ID."
  (unless (and (stringp id) (string-match-p "\\`t[13]_[a-z0-9]+\\'" id))
    (error "Invalid Reddit article ID"))
  (format "<%s@reddit.invalid>" id))

(defun nnreddit--safe-header (value)
  "Return VALUE without header control characters."
  (replace-regexp-in-string "[\x00-\x1f\x7f]+" " " (or value "")))

(defun nnreddit--import (store group records &optional complete-root targets)
  "Import RECORDS as separate articles into GROUP.
When COMPLETE-ROOT is set, remove cached content for missing comments.
TARGETS is a list of notification comment fullnames.  Return new numbers."
  (let ((present (make-hash-table :test #'equal)) new)
    (dolist (record records)
      (let* ((id (plist-get record :id))
             (root (plist-get record :root))
             (parent (plist-get record :parent))
             (old (nnreddit--entry group id))
             (old-content (and old (mapcar (lambda (key) (plist-get old key))
                                           '(:body :html :images))))
             (entry (or old (list :number (plist-get group :next) :id id
                                  :root nil :parent nil :title nil :author nil
                                  :body nil :html nil :images nil
                                  :created nil :url nil
                                  :subreddit nil :deleted nil :target nil))))
        (nnreddit--message-id id)
        (nnreddit--message-id root)
        (when parent (nnreddit--message-id parent))
        (when complete-root
          (unless (equal root complete-root)
            (error "Reddit returned an article from another thread")))
        (puthash id t present)
        (unless old
          (cl-incf (plist-get group :next))
          (push entry (plist-get group :entries))
          (push (plist-get entry :number) new))
        (dolist (key '(:root :parent :title :author :body :html :images
                       :created :url :subreddit :deleted))
          (setf (plist-get entry key) (plist-get record key)))
        (when (and old
                   (not (equal old-content
                               (mapcar (lambda (key) (plist-get entry key))
                                       '(:body :html :images)))))
          (gnus-backlog-remove-article
           (gnus-group-prefixed-name (plist-get group :name)
                                     (list 'nnreddit nnreddit--server))
           (plist-get entry :number)))
        (when (and targets (member id targets))
          (setf (plist-get entry :target) t))
        ;; `plist-put' returns a new head when adding a key to a legacy cache
        ;; record.  Replace the stored cell so new HTML/image fields persist.
        (when old
          (setf (plist-get group :entries)
                (cl-substitute entry old (plist-get group :entries)
                               :test #'eq)))))
    (when complete-root
      (dolist (entry (plist-get group :entries))
        (when (and (equal (plist-get entry :root) complete-root)
                   (not (gethash (plist-get entry :id) present)))
          (setf (plist-get entry :body) ""
                (plist-get entry :html) nil
                (plist-get entry :images) nil
                (plist-get entry :author) "[unavailable]"
                (plist-get entry :deleted) t))))
    (nnreddit--save store)
    (nreverse new)))

(defun nnreddit--visible-p (group entry)
  "Return whether ENTRY appears in GROUP's normal Gnus summary."
  (or (not (equal (plist-get group :kind) "notifications"))
      (null (plist-get entry :parent))
      (plist-get entry :target)))

(defun nnreddit--header (entry)
  "Create a Gnus header for ENTRY."
  (let* ((parent (plist-get entry :parent))
         (title (nnreddit--safe-header (plist-get entry :title)))
         (subject (if parent (concat "Re: " (if (string-empty-p title) "Reddit comment" title))
                    (if (string-empty-p title) "Reddit post" title)))
         (author (nnreddit--safe-header (plist-get entry :author)))
         (timestamp (plist-get entry :created)))
    (make-full-mail-header
     (plist-get entry :number) subject
     (format "%s <%s@reddit.invalid>" author
             (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" author))
     (if (numberp timestamp)
         (format-time-string "%a, %d %b %Y %T %z" (seconds-to-time timestamp) t)
       "Thu, 01 Jan 1970 00:00:00 +0000")
     (nnreddit--message-id (plist-get entry :id))
     (if parent (nnreddit--message-id parent) "") 0 0 "" nil)))

(defun nnreddit--insert-article-body (entry)
  "Insert ENTRY's MIME body, using Reddit HTML when available."
  (let* ((images (plist-get entry :images))
         (plain (or (plist-get entry :body) ""))
         (html (nnreddit--article-html entry)))
    (when (and images (string-empty-p plain))
      (setq plain (string-join images "\n")))
    (if html
        (insert "Content-Type: text/html; charset=utf-8\n"
                "Content-Transfer-Encoding: base64\n\n"
                (base64-encode-string (encode-coding-string html 'utf-8))
                "\n")
      (insert "Content-Type: text/plain; charset=utf-8\n"
              "Content-Transfer-Encoding: base64\n\n"
              (base64-encode-string (encode-coding-string plain 'utf-8))
              "\n"))))

(deffoo nnreddit-retrieve-headers (articles &optional group server _fetch-old)
  (let ((data (nnreddit--group (nnreddit--select server) group)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (number articles)
        (when-let* ((entry (and data (nnreddit--entry data number)))
                    ((nnreddit--visible-p data entry)))
          (nnheader-insert-nov (nnreddit--header entry))))))
  'nov)

(deffoo nnreddit-request-group (group &optional server _dont-check _info)
  (let ((data (nnreddit--group (nnreddit--select server) group)))
    (if (not data) (nnheader-report 'nnreddit "Unknown subscription")
      (nnheader-insert "211 %d 1 %d %s\n"
                       (cl-count-if (lambda (entry) (nnreddit--visible-p data entry))
                                    (plist-get data :entries))
                       (1- (plist-get data :next)) group t))))
(deffoo nnreddit-close-group (_group &optional _server) t)
(deffoo nnreddit-request-list (&optional server)
  (let ((store (nnreddit--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (nnreddit--db-groups store))
        (insert (format "%s %d 1 y\n" (plist-get group :name)
                        (1- (plist-get group :next)))))))
  t)
(deffoo nnreddit-retrieve-groups (_groups &optional server)
  (nnreddit-request-list server) 'active)
(deffoo nnreddit-request-list-newsgroups (&optional server)
  (let ((store (nnreddit--select server)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (nnreddit--db-groups store))
        (insert (plist-get group :name) "\tReddit " (plist-get group :kind) "\n"))))
  t)
(deffoo nnreddit-request-create-group (group &optional server _args)
  (nnreddit--ensure-group (nnreddit--select server) group) t)
(deffoo nnreddit-request-type (_group &optional _article) 'post)
(deffoo nnreddit-asynchronous-p () nil)

(deffoo nnreddit-request-thread (header group)
  (let* ((data (nnreddit--group (nnreddit--select) group))
         (entry (and data (nnreddit--entry data
                                           (when (string-match "<\\(t[13]_[a-z0-9]+\\)@reddit\\.invalid>"
                                                               (mail-header-id header))
                                             (match-string 1 (mail-header-id header))))))
         (root (plist-get entry :root)))
    (when root
      (mapcar #'nnreddit--header
              (sort (cl-remove-if-not
                     (lambda (item) (equal root (plist-get item :root)))
                     (copy-sequence (plist-get data :entries)))
                    (lambda (a b) (< (plist-get a :number) (plist-get b :number))))))))

(deffoo nnreddit-request-article (article &optional group server buffer)
  (let* ((data (nnreddit--group (nnreddit--select server) group))
         (entry (and data (nnreddit--entry data article))))
    (if (not entry) (nnheader-report 'nnreddit "No such cached article")
      (let ((header (nnreddit--header entry)))
        (with-current-buffer (or buffer nntp-server-buffer)
          (erase-buffer)
          (insert "From: " (rfc2047-encode-string (mail-header-from header)) "\n"
                  "Subject: " (rfc2047-encode-string (mail-header-subject header)) "\n"
                  "Date: " (mail-header-date header) "\n"
                  "Message-ID: " (mail-header-id header) "\n"
                  "References: " (mail-header-references header) "\n"
                  "Newsgroups: " group "\n"
                  "Archived-at: <" (nnreddit--safe-header (plist-get entry :url)) ">\n"
                  "MIME-Version: 1.0\n")
          (nnreddit--insert-article-body entry)))
      (cons group (plist-get entry :number)))))

(defun nnreddit-update (group &optional server)
  "Fetch GROUP from Reddit and import each post or reply as an article."
  (let* ((store (nnreddit--select (or server nnreddit--server)))
         (data (nnreddit--ensure-group store group))
         (kind (plist-get data :kind)))
    (pcase kind
      ("subreddit"
       (nnreddit--import store data
                         (plist-get (nnreddit--operation
                                     (list :action "subreddit"
                                           :subreddit (substring group 10))) :posts)))
      ("post"
       (let ((root (concat "t3_" (substring group 5))))
         (nnreddit--import store data
                           (plist-get (nnreddit--operation
                                       (list :action "thread" :post_id (substring root 3)))
                                      :articles)
                           root)))
      ("notifications"
       (dolist (thread (plist-get (nnreddit--operation (list :action "notifications")) :threads))
         (let ((root (concat "t3_" (plist-get thread :post_id))))
           (nnreddit--import store data (plist-get thread :articles)
                             root (plist-get thread :targets))))))
    t))

(deffoo nnreddit-request-scan (&optional group server)
  (let ((store (nnreddit--select server)))
    (dolist (name (if group (list group)
                    (mapcar (lambda (item) (plist-get item :name))
                            (nnreddit--db-groups store))))
      (condition-case problem
          (nnreddit-update name server)
        (error (nnheader-report 'nnreddit "%s" (error-message-string problem))))))
  t)

(defun nnreddit--subscribe (name)
  "Create NAME and open its Gnus summary."
  (unless (gnus-alive-p) (gnus-no-server))
  (let* ((method (list 'nnreddit nnreddit--server))
         (full (gnus-group-prefixed-name name method)))
    (nnreddit--ensure-group (nnreddit--select nnreddit--server) name)
    (with-current-buffer gnus-group-buffer
      (unless (gnus-group-entry full) (gnus-group-make-group name method)))
    (nnreddit-update name nnreddit--server)
    (with-current-buffer gnus-group-buffer
      (gnus-group-read-group t t full))))

;;;###autoload
(defun nnreddit-subscribe-subreddit (name)
  "Subscribe to new posts in subreddit NAME."
  (interactive "sSubreddit name (without r/): ")
  (unless (string-match-p "\\`[A-Za-z0-9_]+\\'" name)
    (user-error "Invalid subreddit name"))
  (nnreddit--subscribe (concat "subreddit." name)))

;;;###autoload
(defun nnreddit-subscribe-post (url)
  "Subscribe to one Reddit post URL."
  (interactive (list (read-string "Reddit post URL: " (thing-at-point 'url t))))
  (unless (and (stringp url)
               (string-match "\\`https://\\(?:www\\.\\|old\\.\\)?reddit\\.com/\\(?:r/[A-Za-z0-9_]+/\\)?comments/\\([a-z0-9]+\\)" url))
    (user-error "Expected a Reddit comments URL"))
  (nnreddit--subscribe (concat "post." (match-string 1 url))))

;;;###autoload
(defun nnreddit-subscribe-notifications ()
  "Subscribe to replies directed at the authenticated Reddit account."
  (interactive)
  (nnreddit--subscribe "notifications"))

(defun nnreddit--context ()
  "Return (STORE GROUP ENTRY SERVER) for the current summary article."
  (unless (derived-mode-p 'gnus-summary-mode) (user-error "Use a Gnus summary"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name))
         (name (gnus-group-real-name gnus-newsgroup-name)))
    (unless (eq (car method) 'nnreddit) (user-error "Not a Reddit group"))
    (let* ((store (nnreddit--select (cadr method)))
           (data (nnreddit--group store name))
           (entry (nnreddit--entry data (gnus-summary-article-number))))
      (unless entry (user-error "No Reddit article here"))
      (list store data entry (cadr method)))))

(defun nnreddit-read-thread ()
  "Fetch this discussion and open its Gnus articles in gnus-thread-reader."
  (interactive)
  (pcase-let* ((`(,store ,data ,entry ,_server) (nnreddit--context))
               (root (plist-get entry :root))
               (number (plist-get entry :number)))
    (nnreddit--import store data
                      (plist-get (nnreddit--operation
                                  (list :action "thread" :post_id (substring root 3)))
                                 :articles) root)
    (gnus-summary-rescan-group t)
    (gnus-summary-goto-subject number t)
    (gnus-summary-refer-thread nil)
    (if (require 'gnus-thread-reader nil t)
        (gnus-thread-reader-open)
      (gnus-summary-show-article))))

(defun nnreddit-refresh ()
  "Update this Reddit group and rescan the Gnus summary."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Use a Gnus summary"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name))
         (name (gnus-group-real-name gnus-newsgroup-name)))
    (unless (eq (car method) 'nnreddit) (user-error "Not a Reddit group"))
    (nnreddit-update name (cadr method))
    (gnus-summary-rescan-group t)))

(defvar nnreddit-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'nnreddit-read-thread)
    (define-key map (kbd "C-c C-t") #'nnreddit-read-thread)
    (define-key map (kbd "G") #'nnreddit-refresh)
    map))
(define-minor-mode nnreddit-summary-mode
  "Reddit summary keys for refreshing and opening a complete reply tree."
  :lighter " Reddit" :keymap nnreddit-summary-mode-map)
(defun nnreddit--summary-setup ()
  "Enable Reddit bindings in a matching Gnus summary."
  (when (and gnus-newsgroup-name
             (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit))
    (setq-local gnus-thread-hide-subtree t)
    (nnreddit-summary-mode 1)))
(add-hook 'gnus-summary-prepare-hook #'nnreddit--summary-setup)

(defun nnreddit--post-body ()
  "Return the plain text Message body, rejecting MIME attachments."
  (let ((mm-decrypt-option 'never) (mm-verify-option 'never) handle)
    (unwind-protect
        (progn
          (setq handle (mm-dissect-buffer t))
          (unless (and (bufferp (car handle))
                       (equal (mm-handle-media-type handle) "text/plain"))
            (error "Reddit posts support plain text only"))
          (let ((text (decode-coding-string (mm-get-part handle)
                                            (or (mm-charset-to-coding-system
                                                 (mail-content-type-get
                                                  (mm-handle-type handle) 'charset))
                                                'utf-8))))
            (when (string-empty-p (string-trim text)) (error "Empty Reddit post"))
            text))
      (when handle (mm-destroy-parts handle)))))

(defun nnreddit--send (store data parent title body)
  "Send BODY to PARENT or post TITLE; retain an uncertain-send lock."
  (let* ((identity (list (plist-get data :name) (plist-get parent :id) title body))
         (fingerprint (secure-hash 'sha256 (prin1-to-string identity)))
         (old (cl-find fingerprint (nnreddit--db-sends store)
                       :key (lambda (item) (plist-get item :fingerprint)) :test #'equal))
         (attempt (or old (list :fingerprint fingerprint :state "pending")))
         result)
    (when (and old (equal (plist-get old :state) "pending"))
      (error "Previous send is uncertain; check Reddit before retrying"))
    (if (and old (equal (plist-get old :state) "sent")) t
      (unless old (push attempt (nnreddit--db-sends store)))
      (setf (plist-get attempt :state) "pending")
      (nnreddit--save store)
      (setq result (nnreddit--operation
                    (if parent
                        (list :action "reply" :parent (plist-get parent :id) :body body)
                      (list :action "submit" :subreddit (substring (plist-get data :name) 10)
                            :title title :body body))))
      (let ((article (plist-get result :article)))
        (unless (and (plist-get article :id)
                     (equal (plist-get article :parent) (plist-get parent :id)))
          (error "Reddit did not confirm the new article; check the website"))
        (nnreddit--import store data (list article))
        (setf (plist-get attempt :state) "sent")
        (nnreddit--save store)
        t))))

(deffoo nnreddit-request-post (&optional server)
  "Post through Reddit's Data API; leave uncertain submissions locked."
  (condition-case problem
      (let* ((store (nnreddit--select server))
             (name (message-fetch-field "newsgroups"))
             (refs (split-string (or (message-fetch-field "references") "")))
             (group (and name (gnus-group-real-name name)))
             (data (and group (nnreddit--group store group)))
             (parent-id (car (last refs)))
             (parent (and data parent-id
                          (when (string-match "\\`<\\(t[13]_[a-z0-9]+\\)@reddit\\.invalid>\\'"
                                              parent-id)
                            (nnreddit--entry data (match-string 1 parent-id)))))
             (title (nnreddit--safe-header (message-fetch-field "subject"))))
        (unless (and data (not (string-match-p "[,\r\n]" name))
                     (or parent (and (equal (plist-get data :kind) "subreddit")
                                     (null parent-id) (not (string-empty-p title)))))
          (error "Reply to a known Reddit article or compose in a subreddit group"))
        (nnreddit--send store data parent title (nnreddit--post-body)))
    (error (nnheader-report 'nnreddit "%s" (error-message-string problem)))))

(gnus-declare-backend "nnreddit" 'post)
(nnoo-define-skeleton nnreddit)
(provide 'nnreddit)
;;; nnreddit.el ends here
