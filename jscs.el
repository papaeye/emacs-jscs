;;; jscs.el --- Consistent JavaScript editing using JSCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: languages, convenience
;; Version: 0.2.0alpha
;; Homepage: https://github.com/papaeye/emacs-jscs
;; Package-Requires: ((emacs "24.1") (langfmt "0.2.0alpha"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; jscs.el provides consistent JavaScript editing using JSCS.
;;
;; Installation:
;;
;; 1. Install JSCS <http://jscs.info/>.
;;
;; 2. Put jscs.el and langfmt.el <https://github.com/papaeye/go-mode.el>
;;    somewhere in your `load-path'.
;;
;; 3. Add the following code into your .emacs:
;;
;;     (autoload 'jscs-indent-apply "jscs" nil t)
;;     (autoload 'jscs-fix "jscs" nil t)
;;     (autoload 'jscs-fix-before-save "jscs" nil t)
;;
;; Usage:
;;
;; To apply JSCS indentation rules to JavaScript modes,
;; add the following code into your .emacs:
;;
;;     (with-eval-after-load 'js
;;       (add-hook 'js-mode-hook #'jscs-indent-apply))
;;
;;     (with-eval-after-load 'js2-mode
;;       (add-hook 'js2-mode-hook #'jscs-indent-apply))
;;
;; To run "jscs --fix" interactively, run \\[jscs-fix].
;;
;; To run "jscs --fix" on the current buffer when saving,
;; add the following code into your .emacs:
;;
;;     (add-hook 'before-save-hook #'jscs-fix-before-save)

;;; Code:

(require 'json)

(require 'langfmt)

(defvar js-indent-level)
(defvar js2-basic-offset)

;; JSCS exit codes
(defconst jscs-exit-code-style-errors 2)
(defconst jscs-exit-missing-config 4)

(defgroup jscs nil
  "Consistent JavaScript editing using JSCS"
  :group 'tools)

(defcustom jscs-command "jscs"
  "The 'jscs' command."
  :type 'string
  :group 'jscs)

(defcustom jscs-node-command "node"
  "The 'node' command."
  :type 'string
  :group 'jscs)

(defcustom jscs-node-path (expand-file-name
			   "../lib/node_modules/"
			   (file-name-directory (executable-find jscs-command)))
  "The NODE_PATH environment variable."
  :type 'string
  :group 'jscs)

(defun jscs--load-config ()
  (with-temp-buffer
    (insert
     "var configFile = require('jscs/lib/cli-config');"
     "var Configuration = require('jscs/lib/config/configuration');"
     "var content = configFile.load();"
     "var config = new Configuration();"
     "config.registerDefaultRules();"
     "config.registerDefaultPresets();"
     "config.load(content);"
     "var result = config.getProcessedConfig();"
     "console.log(JSON.stringify(result));")
    (let ((process-environment process-environment))
      (push (concat "NODE_PATH=" jscs-node-path) process-environment)
      (call-process-region (point-min) (point-max) jscs-node-command t t))
    (goto-char (point-min))
    (ignore-errors
      (json-read))))

(defun jscs-indent--rule-validate-indentation (config)
  (let ((indent (cdr (assq 'validateIndentation config))))
    (when (consp indent)
      (setq indent (cdr (assq 'value indent))))
    (cond
     ((integerp indent)
      (cond
       ((memq major-mode '(js-mode json-mode))
	(setq-local js-indent-level indent))
       ((eq major-mode 'js2-mode)
	(setq-local js2-basic-offset indent)))
      (setq indent-tabs-mode nil))
     ((string= indent "\t")
      (setq indent-tabs-mode t)))))

(defun jscs-indent--rule-maximum-line-length (config)
  (let ((rule (cdr (assq 'maximumLineLength config)))
	tab-size)
    (when (consp rule)
      (setq tab-size (cdr (assq 'tabSize rule)))
      (when (integerp tab-size)
	(setq tab-width tab-size)))))

(defvar jscs-indent--rule-functions
  (list #'jscs-indent--rule-validate-indentation
	#'jscs-indent--rule-maximum-line-length))

;;;###autoload
(defun jscs-indent-apply ()
  "Apply JSCS indentation rules."
  (interactive)
  (let ((config (jscs--load-config)))
    (dolist (func jscs-indent--rule-functions)
      (funcall func config))))

;;;###autoload (autoload 'jscs-fix "jscs" nil t)
;;;###autoload (autoload 'jscs-fix-before-save "jscs" nil t)
(define-langfmt jscs-fix
  "Format the current buffer according to the JSCS tool."
  :group 'jscs
  :modes '(js-mode js2-mode js3-mode)
  :format-command jscs-command
  :format-args '("--fix" "--reporter" "inline")
  :after-format #'jscs-fix--after-format
  :after-diff #'jscs-fix--after-diff
  :error-filter #'jscs-fix--error-filter)

(defun jscs-fix--after-format (context)
  (let ((exit-status (plist-get context :exit-status)))
    (cond
     ((= exit-status jscs-exit-missing-config)
      (message "No configuration found"))
     ((and (/= exit-status 0)
	   (/= exit-status jscs-exit-code-style-errors))
      (message "Could not apply jscs-fix")))))

(defun jscs-fix--after-diff (context)
  (let ((exit-status (plist-get context :exit-status))
	(no-diff-p (plist-get context :no-diff-p)))
    (if no-diff-p
	(message (if (zerop exit-status)
		     "Buffer is already jscs-fixed"
		   "Could not apply jscs-fix"))
      (message (if (zerop exit-status)
		   "Applied jscs-fix"
		 "Applied jscs-fix partially")))))

(defun jscs-fix--error-filter (filename tmpfile)
  (while (search-forward-regexp
	  (concat "^\\(?:"
		  (regexp-quote tmpfile)
		  "\\): line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)")
	  nil t)
    (replace-match (concat (file-name-nondirectory filename)
			   ":" (match-string 1) ":" (match-string 2)
			   ": " (match-string 3))
		   t t)))

(provide 'jscs)
;;; jscs.el ends here
