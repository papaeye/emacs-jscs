;;; jscs.el --- Consistent JavaScript editing using JSCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: languages, convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-jscs
;; Package-Requires: ((emacs "24.1") (langfmt "0.1.0"))

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

(defgroup jscs nil
  "Consistent JavaScript editing using JSCS"
  :group 'tools)

(defcustom jscs-command "jscs"
  "The 'jscs' command."
  :type 'string
  :group 'jscs)

(defvar jscs--presets-path
  (expand-file-name "../lib/node_modules/jscs/presets"
		    (file-name-directory (executable-find jscs-command))))

(defun jscs--read-jscsrc ()
  (let ((dir (locate-dominating-file default-directory ".jscsrc")))
    (when dir
      (json-read-file (expand-file-name ".jscsrc" dir)))))

(defun jscs--read-preset (name)
  (let ((preset (expand-file-name (concat name ".json") jscs--presets-path)))
    (if (file-readable-p preset)
	(json-read-file preset)
      (error "Preset %s is not found" name))))

(defun jscs--config-list (config)
  (let ((preset (cdr (assq 'preset config))))
    (if (stringp preset)
	(cons config (jscs--config-list (jscs--read-preset preset)))
      (list config))))

(defun jscs-indent--rule-validate-indentation (config)
  (let ((indent (cdr (assq 'validateIndentation config))))
    (prog1 indent
      (when (listp indent)
	(setq indent (cdr (assq 'value indent))))
      (cond
       ((integerp indent)
	(cond
	 ((memq major-mode '(js-mode json-mode))
	  (setq-local js-indent-level indent))
	 ((eq major-mode 'js2-mode)
	  (setq js2-basic-offset indent)))
	(setq indent-tabs-mode nil))
       ((string= indent "\t")
	(setq indent-tabs-mode t))))))

(defun jscs-indent--rule-maximum-line-length (config)
  (let ((rule (cdr (assq 'maximumLineLength config)))
	tab-size)
    (prog1 rule
      (when (listp rule)
	(setq tab-size (cdr (assq 'tabSize rule)))
	(when (integerp tab-size)
	  (setq tab-width tab-size))))))

(defvar jscs-indent--rule-functions
  (list #'jscs-indent--rule-validate-indentation
	#'jscs-indent--rule-maximum-line-length))

(defun jscs-indent--apply (config)
  (let ((config-list (jscs--config-list config)))
    (dolist (func jscs-indent--rule-functions)
      (let ((tail config-list)
	    done)
	(while (and (not done) tail)
	  (setq done (funcall func (car tail)))
	  (setq tail (cdr tail)))))))

;;;###autoload
(defun jscs-indent-apply ()
  (interactive)
  (let ((jscsrc (jscs--read-jscsrc)))
    (when jscsrc
      (jscs-indent--apply jscsrc))))

;;;###autoload (autoload 'jscs-fix "jscs" nil t)
;;;###autoload (autoload 'jscs-fix-before-save "jscs" nil t)
(define-langfmt jscs-fix
  "Format the current buffer according to the JSCS tool."
  :group 'jscs
  :modes '(js-mode js2-mode js3-mode)
  :runner #'jscs-fix--runner
  :error-filter #'jscs-fix--error-filter)

(defun jscs-fix--runner (tmpfile patchbuf errbuf)
  (let ((exit (call-process jscs-command nil errbuf nil
                            "--fix" "--reporter" "inline" tmpfile)))
    (if (= exit 1)
        (progn
          (message "No configuration found")
          (when errbuf
            (langfmt-kill-error-buffer errbuf)))
      (if (zerop (call-process-region (point-min) (point-max) "diff"
                                      nil patchbuf nil "-n" "-" tmpfile))
          (message (if (zerop exit)
                       "Buffer is already jscs-fixed"
                     "Could not apply jscs-fix"))
        (langfmt-apply-rcs-patch patchbuf)
        (message (if (zerop exit)
                     "Applied jscs-fix"
                   "Applied jscs-fix partially")))
      (when errbuf
        (if (zerop exit)
            (langfmt-kill-error-buffer errbuf)
          (jscs-fix--process-errors (buffer-file-name) tmpfile errbuf))))))

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
