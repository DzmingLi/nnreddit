;;; nnreddit-test.el --- Offline tests for nnreddit -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nnreddit)

(ert-deftest nnreddit-gnus-search-finds-cached-posts-and-comments ()
  (let* ((group '(:name "subreddit.emacs" :kind "subreddit"
                  :entries ((:number 4 :title "Org workflows" :author "Ada"
                             :body "Outlining")
                            (:number 9 :title "Org workflows" :author "Bob"
                             :body "Nested reply"))))
         (store (make-nnreddit--db :groups (list group)))
         (engine (make-instance 'gnus-search-nnreddit)))
    (cl-letf (((symbol-function 'gnus-server-to-method)
               (lambda (_) '(nnreddit "reddit")))
              ((symbol-function 'nnreddit--select) (lambda (_) store)))
      (should (equal (gnus-search-run-search
                      engine "nnreddit:reddit" '((query . "nested reply"))
                      '("nnreddit:subreddit.emacs"))
                     [["nnreddit:subreddit.emacs" 9 100]]))
      (should (equal (gnus-search-run-search
                      engine "nnreddit:reddit" '((query . "missing"))
                      '("nnreddit:subreddit.emacs"))
                     [])))))

(defun nnreddit-test--listing (children)
  "Make a Reddit listing with CHILDREN."
  (list :kind "Listing" :data (list :children children)))

(ert-deftest nnreddit-retries-truncated-read-response ()
  (let ((nnreddit-username "example")
        (nnreddit-request-interval 0)
        (calls 0))
    (cl-letf (((symbol-function 'nnreddit--token) (lambda () "test-token"))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (cl-incf calls)
                 (with-current-buffer (generate-new-buffer " *nnreddit-response*")
                   (insert "HTTP/1.1 200 OK\r\n\r\n"
                           (if (= calls 1) "{" "{\"data\":{}}"))
                   (current-buffer)))))
      (should (equal (nnreddit--request 'get "/r/emacs/new.json" nil)
                     '(:data nil)))
      (should (= calls 2)))))

(ert-deftest nnreddit-does-not-retry-ambiguous-post ()
  (let ((nnreddit-username "example")
        (nnreddit-request-interval 0)
        (calls 0))
    (cl-letf (((symbol-function 'nnreddit--token) (lambda () "test-token"))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (cl-incf calls)
                 (with-current-buffer (generate-new-buffer " *nnreddit-response*")
                   (insert "HTTP/1.1 200 OK\r\n\r\n{")
                   (current-buffer)))))
      (should-error (nnreddit--request 'post "/api/comment" nil)
                    :type 'error)
      (should (= calls 1)))))

(ert-deftest nnreddit-thread-keeps-each-comment-as-an-article ()
  (let* ((post '(:kind "t3" :data
                 (:name "t3_abc" :title "Example" :author "writer"
                  :selftext "First post" :subreddit "emacs"
                  :created_utc 100 :permalink "/r/emacs/comments/abc/")))
         (child '(:kind "t1" :data
                  (:name "t1_one" :parent_id "t3_abc" :link_id "t3_abc"
                   :author "first" :body "Reply" :created_utc 101
                   :replies (:kind "Listing" :data
                             (:children ((:kind "t1" :data
                                          (:name "t1_two" :parent_id "t1_one"
                                           :link_id "t3_abc" :author "second"
                                           :body "Nested" :created_utc 102))))))))
         records)
    (cl-letf (((symbol-function 'nnreddit--request)
               (lambda (_method _path _fields)
                 (list (nnreddit-test--listing (list post))
                       (nnreddit-test--listing (list child))))))
      (setq records (nnreddit--thread "abc")))
    (should (= (length records) 3))
    (should (equal (plist-get (cl-find "t1_two" records :test #'equal
                                      :key (lambda (record) (plist-get record :id)))
                             :parent)
                   "t1_one"))
    (should (equal (plist-get (cl-find "t1_one" records :test #'equal
                                      :key (lambda (record) (plist-get record :id)))
                             :root)
                   "t3_abc"))))

(ert-deftest nnreddit-thread-expands-morechildren ()
  (let* ((post '(:kind "t3" :data
                 (:name "t3_abc" :title "Example" :author "writer"
                  :selftext "" :subreddit "emacs" :created_utc 100)))
         (more '(:kind "more" :data (:children ("one") :parent_id "t3_abc")))
         (comment '(:kind "t1" :data
                    (:name "t1_one" :parent_id "t3_abc" :link_id "t3_abc"
                     :author "reader" :body "Expanded" :created_utc 101)))
         paths)
    (cl-letf (((symbol-function 'nnreddit--request)
               (lambda (_method path _fields)
                 (push path paths)
                 (if (equal path "/api/morechildren.json")
                     (list :json (list :data (list :things (list comment))))
                   (list (nnreddit-test--listing (list post))
                         (nnreddit-test--listing (list more)))))))
      (should (= 2 (length (nnreddit--thread "abc")))))
    (should (member "/api/morechildren.json" paths))))

(ert-deftest nnreddit-import-preserves-stable-article-numbers ()
  (let* ((directory (make-temp-file "nnreddit-test-" t))
         (store (make-nnreddit--db :file (expand-file-name "cache.json" directory)))
         (group (list :name "post.abc" :kind "post" :next 1 :entries nil))
         (root '(:id "t3_abc" :root "t3_abc" :title "Example"
                 :author "writer" :body "Root" :created 100))
         (reply '(:id "t1_one" :root "t3_abc" :parent "t3_abc"
                  :title "Example" :author "reader" :body "Reply" :created 101)))
    (unwind-protect
        (progn
          (should (equal (nnreddit--import store group (list root reply)) '(1 2)))
          (should (null (nnreddit--import store group (list root reply))))
          (should (= (plist-get (nnreddit--entry group "t1_one") :number) 2))
          (should (equal (mail-header-references
                          (nnreddit--header (nnreddit--entry group "t1_one")))
                         "<t3_abc@reddit.invalid>")))
      (delete-directory directory t))))

(ert-deftest nnreddit-refresh-adds-html-to-existing-cache-record ()
  (let* ((directory (make-temp-file "nnreddit-cache-" t))
         (store (make-nnreddit--db :file (expand-file-name "cache.json" directory)))
         (old (list :number 1 :id "t3_example" :body "**old**"))
         (group (list :name "subreddit.emacs" :kind "subreddit"
                      :next 2 :entries (list old)))
         (record (list :id "t3_example" :root "t3_example" :body "**new**"
                       :html "<p><strong>new</strong></p>"
                       :images '("https://i.redd.it/example.png")))
         invalidated)
    (unwind-protect
        (progn
          (setf (nnreddit--db-groups store) (list group))
          (cl-letf (((symbol-function 'gnus-backlog-remove-article)
                     (lambda (name number)
                       (push (list name number) invalidated))))
            (nnreddit--import store group (list record)))
          (should (equal invalidated '(("nnreddit:subreddit.emacs" 1))))
          (let ((cached (nnreddit--entry group "t3_example")))
            (should (= (plist-get cached :number) 1))
            (should (equal (plist-get cached :html)
                           "<p><strong>new</strong></p>"))
            (should (equal (plist-get cached :images)
                           '("https://i.redd.it/example.png"))))
          (should (equal
                   (plist-get
                    (nnreddit--entry
                     (car (nnreddit--db-groups (nnreddit--load (nnreddit--db-file store))))
                     "t3_example")
                    :images)
                   '("https://i.redd.it/example.png"))))
      (delete-directory directory t))))

(ert-deftest nnreddit-article-renders-reddit-bold-and-inline-image ()
  (let* ((thing '(:data (:name "t3_example" :title "Example" :author "writer"
                         :selftext "**Bold**\n\nhttps://preview.redd.it/x.jpg"
                         :selftext_html
                         "<div class=\"md\"><p><strong>Bold</strong></p><p><a href=\"https://preview.redd.it/x.jpg?width=800&amp;format=pjpg\">https://preview.redd.it/x.jpg?width=800&amp;format=pjpg</a></p></div>"
                         :subreddit "emacs" :created_utc 100)))
         (record (nnreddit--post-record thing))
         (html (nnreddit--article-html record)))
    (should (string-match-p "<strong>Bold</strong>" html))
    (should (string-match-p
             (regexp-quote
              "<img src=\"https://preview.redd.it/x.jpg?width=800&amp;format=pjpg\"")
             html))
    (with-temp-buffer
      (insert "MIME-Version: 1.0\n")
      (nnreddit--insert-article-body record)
      (goto-char (point-min))
      (let ((handle (mm-dissect-buffer t)))
        (unwind-protect
            (progn
              (should (equal (mm-handle-media-type handle)
                             "text/html"))
              (should (string-match-p
                       "<strong>Bold</strong>"
                       (decode-coding-string (mm-get-part handle)
                                             'utf-8))))
          (mm-destroy-parts handle))))))

(ert-deftest nnreddit-gallery-image-renders-without-text-body ()
  (let* ((thing '(:data (:name "t3_gallery" :title "Gallery" :author "writer"
                         :selftext "" :subreddit "emacs" :created_utc 100
                         :gallery_data (:items ((:media_id "first")))
                         :media_metadata (:first (:s (:u "https://i.redd.it/a.png"))))))
         (record (nnreddit--post-record thing)))
    (should (equal (plist-get record :images) '("https://i.redd.it/a.png")))
    (should (string-match-p "<img src=\"https://i.redd.it/a.png\""
                            (nnreddit--article-html record)))))

(ert-deftest nnreddit-send-does-not-repeat-confirmed-submission ()
  (let* ((directory (make-temp-file "nnreddit-send-" t))
         (store (make-nnreddit--db :file (expand-file-name "cache.json" directory)))
         (group (list :name "subreddit.emacs" :kind "subreddit"
                      :next 1 :entries nil))
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nnreddit--operation)
                   (lambda (_request)
                     (cl-incf calls)
                     (list :article
                           (list :id "t3_post" :root "t3_post" :parent nil
                                 :title "Title" :author "writer" :body "Body")))))
          (should (nnreddit--send store group nil "Title" "Body"))
          (should (nnreddit--send store group nil "Title" "Body"))
          (should (= calls 1))
          (should (= (length (plist-get group :entries)) 1)))
      (delete-directory directory t))))

(ert-deftest nnreddit-send-locks-uncertain-submission ()
  (let* ((directory (make-temp-file "nnreddit-send-" t))
         (store (make-nnreddit--db :file (expand-file-name "cache.json" directory)))
         (group (list :name "subreddit.emacs" :kind "subreddit"
                      :next 1 :entries nil))
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nnreddit--operation)
                   (lambda (_request) (cl-incf calls) (error "Simulated timeout"))))
          (should-error (nnreddit--send store group nil "Title" "Body"))
          (should-error (nnreddit--send store group nil "Title" "Body")
                        :type 'error)
          (should (= calls 1)))
      (delete-directory directory t))))

(ert-deftest nnreddit-oauth-url-uses-permanent-installed-app-grant ()
  (let* ((nnreddit-client-id "public_app_id")
         (nnreddit-redirect-uri "http://127.0.0.1:17973")
         (url (nnreddit--authorization-url "chosen-state")))
    (should (string-prefix-p "https://www.reddit.com/api/v1/authorize?" url))
    (should (string-match-p "client_id=public_app_id" url))
    (should (string-match-p "duration=permanent" url))
    (should (string-match-p "state=chosen-state" url))
    (should (string-match-p "response_type=code" url))))

(ert-deftest nnreddit-oauth-callback-rejects-mismatched-state ()
  (let ((nnreddit-redirect-uri "http://127.0.0.1:17973"))
    (cl-letf (((symbol-function 'nnreddit--oauth-post)
               (lambda (_fields) (ert-fail "Must reject before token exchange"))))
      (should-error
       (nnreddit--complete-authorization
        "http://127.0.0.1:17973?state=wrong&code=secret" "expected")))))

(ert-deftest nnreddit-oauth-callback-saves-private-refresh-token ()
  (let* ((directory (make-temp-file "nnreddit-oauth-" t))
         (nnreddit-refresh-token-file (expand-file-name "refresh-token" directory))
         (nnreddit-redirect-uri "http://127.0.0.1:17973")
         (nnreddit--access-token nil)
         fields)
    (unwind-protect
        (cl-letf (((symbol-function 'nnreddit--oauth-post)
                   (lambda (request)
                     (setq fields request)
                     '(:access_token "access" :expires_in 3600
                       :refresh_token "refresh"))))
          (nnreddit--complete-authorization
           "http://127.0.0.1:17973?state=expected&code=one%2Btwo" "expected")
          (should (equal (cadr (assoc "code" fields)) "one+two"))
          (should (equal (with-temp-buffer
                           (insert-file-contents nnreddit-refresh-token-file)
                           (buffer-string))
                         "refresh\n"))
          (should (= #o600 (logand #o777 (file-modes nnreddit-refresh-token-file))))
          (should (equal nnreddit--access-token "access")))
      (delete-directory directory t))))

(ert-deftest nnreddit-oauth-refresh-uses-saved-token ()
  (let* ((directory (make-temp-file "nnreddit-oauth-" t))
         (nnreddit-refresh-token-file (expand-file-name "refresh-token" directory))
         (nnreddit--access-token nil)
         fields)
    (unwind-protect
        (progn
          (with-temp-file nnreddit-refresh-token-file (insert "refresh\n"))
          (cl-letf (((symbol-function 'nnreddit--oauth-post)
                     (lambda (request)
                       (setq fields request)
                       '(:access_token "new-access" :expires_in 3600))))
            (should (equal (nnreddit--token) "new-access"))
            (should (equal fields '(("grant_type" "refresh_token")
                                    ("refresh_token" "refresh"))))))
      (delete-directory directory t))))

(ert-deftest nnreddit-oauth-accepts-browser-canonicalized-loopback-url ()
  (let* ((directory (make-temp-file "nnreddit-oauth-" t))
         (nnreddit-refresh-token-file (expand-file-name "refresh-token" directory))
         (nnreddit-redirect-uri "http://127.0.0.1:17973")
         fields)
    (unwind-protect
        (cl-letf (((symbol-function 'nnreddit--oauth-post)
                   (lambda (request)
                     (setq fields request)
                     '(:access_token "access" :expires_in 3600
                       :refresh_token "refresh"))))
          (nnreddit--complete-authorization
           "http://127.0.0.1:17973/?state=expected&code=one-time-code#_"
           "expected")
          (should (equal (cadr (assoc "code" fields)) "one-time-code"))
          (should (equal (cadr (assoc "redirect_uri" fields))
                         nnreddit-redirect-uri))
          (should (equal (file-modes nnreddit-refresh-token-file) #o600))
          (should-error
           (nnreddit--complete-authorization
            "http://127.0.0.1:17973/?state=wrong&code=other"
            "expected")))
      (delete-directory directory t))))

(ert-deftest nnreddit-oauth-token-request-has-no-account-password ()
  (let ((nnreddit-client-id "public_app_id")
        (nnreddit-username "example")
        seen-url seen-body seen-headers)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _)
                 (setq seen-url url seen-body url-request-data
                       seen-headers url-request-extra-headers)
                 (with-current-buffer (generate-new-buffer " *nnreddit-oauth-test*")
                   (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                           "{\"access_token\":\"access\",\"expires_in\":3600}")
                   (current-buffer)))))
      (should (equal (plist-get
                      (nnreddit--oauth-post
                       '(("grant_type" "refresh_token")
                         ("refresh_token" "saved-token")))
                      :access_token)
                     "access")))
    (should (equal seen-url "https://www.reddit.com/api/v1/access_token"))
    (should (string-match-p "grant_type=refresh_token" seen-body))
    (should-not (string-match-p "password" seen-body))
    (should (equal (cdr (assoc "Authorization" seen-headers))
                   (concat "Basic " (base64-encode-string "public_app_id:" t))))
    (should (string-match-p "nnreddit-emacs:0.1.0 (by /u/example)"
                            (cdr (assoc "User-Agent" seen-headers))))))

(ert-deftest nnreddit-user-agent-validates-username ()
  (let ((nnreddit-username "bad\r\nHeader: injected"))
    (should-error (nnreddit--user-agent) :type 'user-error))
  (let ((nnreddit-username nil))
    (should-error (nnreddit--user-agent) :type 'user-error)))

(provide 'nnreddit-test)
;;; nnreddit-test.el ends here
