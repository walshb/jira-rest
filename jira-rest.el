;;; jira-rest.el -- Interact with JIRA REST API.

;; Copyright (C) 2012 Matt DeBoard

;; This work is incorporates many concepts & code from, and is heavily
;; influenced by jira.el, the work of Brian Zwahr and Dave Benjamin:
;; http://emacswiki.org/emacs/JiraMode

;; Documentation of JIRA REST API can be found at URL:
;; https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

;;; Code:
(require 'cl)
(require 'json)
(require 'url)

;; ********************************
;; JIRA REST Mode - By Matt DeBoard
;; ********************************

(defgroup jira-rest nil
  "JIRA customization group."
  :group 'applications)

(defgroup jira-rest-faces nil
  "Faces for displaying JIRA information."
  :group 'jira)

(defvar jira-rest-auth-info nil
  "The auth header used to authenticate each request. Please
see URL https://developer.atlassian.com/display/JIRADEV/JIRA+REST+API+Example+-+Basic+AuthenticationConsists for more information.")

(defun old-load-auth-info ()
  (let ((jira-pwd-file (expand-file-name "~/.jira-auth-info.el")))
    (if (file-regular-p jira-pwd-file)
        (load jira-pwd-file))))

(defvar jira-rest-username nil
  "JIRA REST username")

(defvar jira-rest-password nil
  "JIRA REST password")

(defvar jira-rest-server nil
  "JIRA REST server URL up to but not including '/rest'")

(defvar jira-rest-proj-id nil
  "JIRA REST project key prefix")

(defun load-auth-info ()
  (unless (> (length jira-rest-password) 0)
    (setq jira-rest-password (read-passwd "JIRA password: "))))

(defun jira-rest-setup-endpoint ()
  (if (or (equal jira-rest-endpoint nil)
          (equal jira-rest-endpoint ""))
      (if jira-rest-server
          (setq jira-rest-endpoint (concat jira-rest-server "/rest/api/latest/"))
        (message "jira-rest-endpoint not set! Please set this
value in .jira-auth-info.el.")
        nil)
    t))

(defun jira-rest-login ()
  (interactive)
  (if (jira-rest-setup-endpoint)
      (if (load-auth-info)
          (let ((enc (base64-encode-string
                      (concat jira-rest-username ":" jira-rest-password))))
            (setq jira-rest-auth-info (concat "Basic " enc)))
        (message "You must provide your login information.")
        nil)))

(defcustom jira-rest-endpoint ""
  "The URL of the REST API endpoint for user's JIRA
 installation, with trailing '/'."
  :group 'jira-rest
  :type 'string
  :initialize 'custom-initialize-set)

(defface jira-rest-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira-rest-faces)

(defface jira-rest-issue-info-header-face
  '((t (:bold t :inherit 'jira-rest-issue-info-face)))
  "Base face for issue headers."
  :group 'jira-rest-faces)

(defface jira-rest-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira-rest-faces)

(defface jira-rest-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira-rest-faces)

(defface jira-rest-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira-rest-faces)

(defface jira-rest-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira-rest-faces)

(defface jira-rest-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira-rest-faces)

(defface jira-rest-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira-rest-faces)

(defvar jira-rest-mode-hook nil)

(defvar jira-rest-mode-map nil)

(if jira-rest-mode-map
    nil
  (progn
    (setq jira-rest-mode-map (make-sparse-keymap))
    (define-key jira-rest-mode-map "c" 'jira-rest-create-issue)
    (define-key jira-rest-mode-map "di" 'jira-rest-delete-issue)
    (define-key jira-rest-mode-map "a" 'jira-rest-change-assignee)
    (define-key jira-rest-mode-map "gg" 'jira-rest-get-watchers)
    (define-key jira-rest-mode-map "ga" 'jira-rest-add-watcher)
    (define-key jira-rest-mode-map "gr" 'jira-rest-remove-watcher)
    (define-key jira-rest-mode-map "\S-q" 'jira-rest-mode-quit)))

(defun jira-rest-mode ()
  "A mode for working with JIRA's JSON REST API. The full
specification for the API can be found at URL
https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

Requires JIRA 5.0 or greater.

\\{jira-rest-mode-map}"
  (interactive)
  (if (jira-rest-login)
      (progn
        (switch-to-buffer "*JIRA-REST*")
        (kill-all-local-variables)
        (setq major-mode 'jira-rest-mode)
        (setq mode-name "JIRA-REST")
        (use-local-map jira-rest-mode-map)
        (run-hooks 'jira-rest-mode-hook)
        ;; (jira-rest-store-projects)
        ;; (jira-rest-store-priorities)
        ;; (jira-rest-store-statuses)
        ;; (jira-rest-store-types)
        (insert "Welcome to jira-rest-mode!")
        (message "jira rest mode loaded!"))))

(defvar jira-rest-current-issue nil
  "This holds the currently selected issue.")

(defvar jira-rest-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira-rest-types nil
  "This holds a list of issues types.")

(defvar jira-rest-statuses nil
  "This holds a list of statuses.")

(defvar jira-rest-priorities nil
  "This holds a list of priorities.")

(defvar jira-rest-user-fullnames nil
  "This holds a list of user fullnames.")

(defvar response nil)

(defun jira-rest-api-interact (method data &optional path)
  "Interact with the API using method 'method' and data 'data'.
Optional arg 'path' may be provided to specify another location further
down the URL structure to send the request."
  (progn
    (if (not jira-rest-auth-info)
        (jira-rest-login))
      ;;(message "You must login first, 'M-x jira-rest-login'.")
    (let* ((url-request-method method)
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,jira-rest-auth-info)))
           (url-request-data data)
           (target (concat jira-rest-endpoint path))
           (buf (url-retrieve-synchronously target)))
      (unless (equal method "DELETE")
        (with-current-buffer buf
          (beginning-of-buffer)
          (let ((data (buffer-substring (search-forward-regexp "^$")
                                        (point-max))))
            (json-read-from-string data)))))))

(defun jira-rest-mode-quit ()
  (interactive)
  (kill-buffer "*JIRA-REST*"))

(defun id-or (s)
  "Return ':id' if 's' is a numeric string. Otherwise, return
nil. The idea here is that the JIRA REST API spec allows the 'project'
and 'issuetype' keys to be either 'id' or some other value (in the
case of 'project', the other is 'key'; for 'issuetype', 'name'). This fn
enables us to allow either type of user input."
  (if (not (equal 0 (string-to-number s)))
      "id"))

(defun jira-rest-create-issue-simple (project summary description issuetype)
  "File a new issue with JIRA."
  (interactive (list (read-string "Project Key: ")
                     (read-string "Summary: ")
                     (read-string "Description: ")
                     (read-string "Issue Type: ")))
  (if (or (equal project "")
          (equal summary "")
          (equal description "")
          (equal issuetype ""))
      (message "Must provide all information!")
    (let ((field-hash (make-hash-table :test 'equal))
          (issue-hash (make-hash-table :test 'equal))
          (project-hash (make-hash-table :test 'equal))
          (issuetype-hash (make-hash-table :test 'equal)))
      ;; Create the JSON string that will be passed to create the ticket.
      (progn
        ;; Populate our hashes, from bottom to top. The format for these
        ;; nested hash tables follow the format outlined in the JIRA REST
        ;; API documentation.
        (puthash (or (id-or project) "key") project project-hash)
        (puthash (or (id-or issuetype) "name") issuetype issuetype-hash)
        (puthash "project" project-hash issue-hash)
        (puthash "issuetype" issuetype-hash issue-hash)
        (puthash "summary" summary issue-hash)
        (puthash "description" description issue-hash)
        (puthash "fields" issue-hash field-hash)
        ;; Return the JSON-encoded hash map.
        (jira-rest-api-interact "POST" (json-encode field-hash) "issue")
        response))))

(defun jira-rest-get-issue (k)
  "File a new issue with JIRA."
  (jira-rest-api-interact "GET" nil (concat "issue/" k)))

(defun jira-rest-create-issue (fields)
  "File a new issue with JIRA."
  (let* ((restdata (list (cons 'fields fields)))
         (res (jira-rest-api-interact "POST" (json-encode restdata) "issue")))
    (message "got result %S" res)
    (cdr (assoc 'key res))))

(defun jira-rest-create-subtask (fields parent-id)
  (let ((allfields (cons (cons 'parent (list (cons 'key parent-id))) fields)))
    (jira-rest-create-issue allfields)))

(defun jira-rest-update-issue (k fields)
  "Update an issue in JIRA."
  (let ((restdata (list (cons 'fields fields))))
    (jira-rest-api-interact "PUT" (json-encode restdata) (concat "issue/" k))))

(defun jira-rest-delete-issue (k)
  "Delete an issue with unique identifier 'k'. 'k' is either an
issueId or key."
  (interactive (list (read-string "Issue Key or ID: ")))
  (jira-rest-api-interact "DELETE" nil (concat "issue/" k)))

(defun jira-rest-get-watchers (k)
  "Get all the watchers for an issue."
  (interactive (list (read-string "Issue Key or ID: ")))
  (jira-rest-api-interact "GET" nil (concat "issue/" k "/watchers")))

(defun jira-rest-add-watcher (k name)
  "Add a watcher to an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "Username to Add as Watcher: ")))
  (jira-rest-api-interact "POST" (json-encode name) (concat "issue/" k "/watchers")))

(defun jira-rest-remove-watcher (k name)
  "Remove a watcher from an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "Username to Remove as Watcher: ")))
  (jira-rest-api-interact "DELETE" nil (concat "issue/" k "/watchers?" name)))

(defun jira-rest-change-assignee (k &optional name)
  "Change the assignee for an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "New Assignee: ")))
  (let ((name-hash (make-hash-table :test 'equal)))
    (progn
      (puthash "name" name name-hash)
      (jira-rest-api-interact "PUT" (json-encode name-hash)
                              (concat "issue/" k "/assignee")))))

(defun jira-rest-get-transitions (k)
  (jira-rest-api-interact "GET" nil (concat "issue/" k "/transitions")))

(setq jira-rest-cached-data (make-hash-table :test 'equal))

(defun jira-get-cached (tag func)
  (let ((res (gethash tag jira-rest-cached-data)))
    (unless res
      (progn
        (setq res (funcall func))
        (puthash tag res jira-rest-cached-data)))
    res))

(defun jira-rest-get-projects ()
  (interactive)
  (jira-get-cached 'projects
                   (lambda ()
                     (jira-rest-api-interact "GET" nil "project"))))

(defun jira-rest-do-jql-search (jql)
  (interactive (list (read-string "JQL: ")))
  (let ((restdata (jira-rest-api-interact "GET" nil (concat "search?jql=" (url-hexify-string jql)))))
    (cdr (assoc 'issues restdata))))

(defun jira-rest-get-statuses ()
  (jira-get-cached 'statuses
                   (lambda ()
                     (let ((restdata (jira-rest-api-interact "GET" nil (concat "project/" jira-rest-proj-id "/statuses"))))
                       restdata))))

(defun jira-rest-get-components (project)
  (jira-get-cached (concat "components/" project)
                   (lambda ()
                     (let ((restdata (jira-rest-api-interact "GET" nil (concat "project/" project "/components"))))
                       (mapcar (lambda (compo)
                                 (cons (cdr (assoc 'id compo)) (cdr (assoc 'name compo))))
                               restdata)))))

(defun jira-rest-get-priorities ()
  (jira-get-cached 'priorities
                   (lambda ()
                     (let ((restdata (jira-rest-api-interact "GET" nil "priority")))
                       (mapcar (lambda (prio)
                                 (cons (cdr (assoc 'id prio)) (cdr (assoc 'name prio))))
                               restdata)))))

(defun jira-rest-get-issue-types-internal ()
  (jira-get-cached 'issue-types
                   (lambda ()
                     (let ((restdata (jira-rest-api-interact "GET" nil "issuetype")))
                       restdata))))

(defun jira-rest-get-issue-types ()
  (mapcar (lambda (elem)
            (cons (cdr (assoc 'id elem)) (cdr (assoc 'name elem))))
          (remove-if (lambda (elem) (equal (cdr (assoc 'subtask elem)) t))
                     (jira-rest-get-issue-types-internal))))

(defun jira-rest-get-subtask-types ()
  (mapcar (lambda (elem)
            (cons (cdr (assoc 'id elem)) (cdr (assoc 'name elem))))
          (remove-if-not (lambda (elem) (equal (cdr (assoc 'subtask elem)) t))
                         (jira-rest-get-issue-types-internal))))

(defun jira-rest-get-resolutions ()
  (jira-get-cached 'resolutions
                   (lambda ()
                     (let ((restdata (jira-rest-api-interact "GET" nil "resolution")))
                       (mapcar (lambda (reso)
                                 (cons (cdr (assoc 'id reso)) (cdr (assoc 'name reso))))
                               restdata)))))

;;    (cdr (assoc 'statuses restdata))))

(defun jira-rest-get-issue-regexp ()
  "Return a regexp that will match an issue id.

The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (concat "\\<" jira-rest-proj-id "-[0-9]+\\>"))

(defun jira-rest-make-list (data field)
  "Map all assoc elements in DATA to the value of FIELD in that element."
  (mapcar (lambda (elem)
            (cdr (assoc field elem)))
          data))

(provide 'jira-rest)
;;; jira-rest.el ends here
