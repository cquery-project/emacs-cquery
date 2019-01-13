;;; cquery.el --- cquery client for lsp-mode     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Author:  Tobias Pisani
;; Package-Version: 20180122.1
;; Version: 0.1
;; Homepage: https://github.com/jacobdufault/cquery
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (dash "0.13"))
;; Keywords: languages, lsp, c++

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:

;;
;; To enable, call (lsp-cquery-enable) in your c++-mode hook.
;;

;;; Code:

(require 'cquery-common)
(require 'cquery-semantic-highlighting)
(require 'cquery-code-lens)
(require 'cquery-tree)
(require 'cquery-call-hierarchy)
(require 'cquery-inheritance-hierarchy)

(require 'seq)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom cquery-executable
  "cquery"
  "Path of the cquery executable."
  :type 'file
  :group 'cquery)

(defcustom cquery-extra-args
  nil
  "Additional command line options passed to the cquery executable."
  :type '(repeat string)
  :group 'cquery)

(defalias 'cquery-additional-arguments 'cquery-extra-args)

(defcustom cquery-cache-dir
  ".cquery_cached_index/"
  "Directory in which cquery will store its index cache.
Relative to the project root directory."
  :type 'directory
  :group 'cquery)

(defcustom cquery-extra-init-params
  nil
  "Additional initializationOptions passed to cquery."
  :type '(repeat string)
  :group 'cquery)

(defcustom cquery-project-root-function
  nil
  "A function used to find the project root.
"
  :type '(repeat function)
  :group 'cquery)

(defcustom cquery-cache-dir-function #'cquery-cache-dir-in-project
  "A function returning the directory to store cache files given the project path."
  :type '(radio
          (function-item cquery-cache-dir-in-project)
          (function-item cquery-cache-dir-consolidated))
  :group 'cquery)

(defcustom cquery-cache-dir-consolidated-path
  (expand-file-name "cquery-cache.d" user-emacs-directory)
  "Root path used by cquery-cache-dir-consolidated .

Cquery cache files will be stored here in separate directories for each project.
By default, this is a directory in the user's emacs.d directory, but it can be any
absolute path."
  :type 'string
  :group 'cquery)

(defcustom cquery-project-roots
  nil
  "A list of project roots that will be matched against the source filename first
to get the project root.

This is used by `cquery-project-roots-matcher', useful when your project has
subprojects. Otherwise `projectile' and `project' may think the file resides
in a subproject and thus does not belong to the current workspace.
"
  :type '(repeat directory)
  :group 'cquery)

(defcustom cquery-project-root-matchers
  '(cquery-project-roots-matcher projectile-project-root "compile_commands.json" ".cquery" "build/compile_commands.json")
  "List of matchers that are used to locate the cquery project roots.
Each matcher is run in order, and the first successful (non-nil) matcher
determines the project root.

A `string' entry defines a dominating file that exists in either the
current working directory or a parent directory. cquery will traverse
upwards through the project directory structure and return the first
matching file.

A `function' entry define a callable function that either returns workspace's
root location or `nil' if subsequent matchers should be used instead.
"
  :type '(repeat (choice (file) (function)))
  :group 'cquery)

;; ---------------------------------------------------------------------
;;   Other cquery-specific methods
;; ---------------------------------------------------------------------

(defun cquery-file-info ()
  (lsp--cur-workspace-check)
  (lsp--send-request
   (lsp--make-request "$cquery/fileInfo"
                      `(:textDocument ,(lsp--text-document-identifier)))))

(defun cquery-preprocess-file (&optional output-buffer)
  "Preprocess selected buffer."
  (interactive)
  (lsp--cur-workspace-check)
  (-when-let* ((mode major-mode)
               (info (cquery-file-info))
               (args (seq-into (gethash "args" info) 'vector))
               (new-args (let ((i 0) ret)
                           (while (< i (length args))
                             (let ((arg (elt args i)))
                               (cond
                                ((string= arg "-o") (cl-incf i))
                                ((string-match-p "\\`-o.+" arg))
                                (t (push arg ret))))
                             (cl-incf i))
                           (nreverse ret))))
    (with-current-buffer (or output-buffer
                             (get-buffer-create
                              (format "*lsp-cquery preprocess %s*" (buffer-name))))
      (pop-to-buffer (current-buffer))
      (with-silent-modifications
        (erase-buffer)
        (insert (format "// Generated by: %s"
                        (combine-and-quote-strings new-args)))
        (insert (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'process-file (car new-args) nil t nil "-E" (cdr new-args)))))
        (delay-mode-hooks (funcall mode))
        (setq buffer-read-only t)))))

(defun cquery-freshen-index (&optional whitelist blacklist)
  "Rebuild indexes for matched files.
`whitelist' and `blacklist' are ECMAScript regex used by std::regex_match
`regexp-quote' quotes in elisp flavored regex, so some metacharacters may fail."
  (interactive (list (list (concat "^" (regexp-quote buffer-file-name) "$")) (list ".")))
  (lsp--cur-workspace-check)
  (lsp--send-notification
   (lsp--make-notification "$cquery/freshenIndex"
                           (list :whitelist (or whitelist [])
                                 :blacklist (or blacklist [])))))

(defun cquery-xref-find-custom (method &optional display-action)
  "Find cquery-specific cross references.

Choices of METHOD include \"$cquery/base\", \"$cquery/callers\",
\"$cquery/derived\", \"$cquery/vars\".
Read document for all choices. DISPLAY-ACTION is passed to xref--show-xrefs."
  (lsp--cur-workspace-check)
  (let ((xrefs (lsp--locations-to-xref-items
                (lsp--send-request
                 (lsp--make-request method
                                    (lsp--text-document-position-params))))))
    (unless xrefs
      (user-error "No %s found" method))
    (xref--show-xrefs xrefs display-action)))


(defun cquery-cache-dir-in-project (proj-dir)
  "Return project relative cache directory (see cquery-cache-dir-function).

The name of the project-relative directory used for this is given by cquery-cache-dir."
  (expand-file-name cquery-cache-dir proj-dir))

(defun cquery-cache-dir-consolidated (proj-dir)
  "Return consolidated cache directory path. (see cquery-cache-dir-function).

The place is given by cquery-cache-dir-consolidated-path."
  (expand-file-name
   (replace-regexp-in-string "\/" "!" (directory-file-name proj-dir) t t nil 1)
   cquery-cache-dir-consolidated-path))

;; ---------------------------------------------------------------------
;;  Register lsp client
;; ---------------------------------------------------------------------

; TODO: prog reports for modeline
(defun cquery--get-init-params ()
  `(,@cquery-extra-init-params
    :cacheDirectory ,(file-name-as-directory
                      (funcall cquery-cache-dir-function (lsp--suggest-project-root)))
    :highlight (:enabled ,(or (and cquery-sem-highlight-method t) :json-false))
    :emitInactiveRegions ,(or cquery-enable-inactive-region :json-false)))

(defun cquery--suggest-project-root ()
  (and (memq major-mode '(c-mode c++-mode objc-mode))
       (cquery--get-root)))

(advice-add 'lsp--suggest-project-root :before-until #'cquery--suggest-project-root)

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection (lambda () (cons cquery-executable cquery-extra-args)))
  :major-modes '(c-mode c++-mode objc-mode)
  :server-id 'cquery
  :multi-root nil
  :notification-handlers
  (lsp-ht
   ("$cquery/progress" #'ignore)
   ("$cquery/setInactiveRegions" #'cquery--set-inactive-regions)
   ("$cquery/publishSemanticHighlighting" #'cquery--publish-semantic-highlighting))
  :initialization-options #'cquery--get-init-params
  :library-folders-fn nil))

(provide 'cquery)
;;; cquery.el ends here
