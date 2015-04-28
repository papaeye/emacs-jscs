;;; jscs.el --- Consistent JavaScript editing using JSCS  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: languages, convenience
;; Version: 0.1.0
;; Homepage: https://github.com/papaeye/emacs-jscs
;; Package-Requires: ((emacs "24.1"))

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
;; 2. Put jscs.el somewhere in your `load-path'.
;;
;; 3. Add the following code into your .emacs:
;;
;;     (autoload 'jscs-indent-apply "jscs" nil t)
;;
;;     (with-eval-after-load 'js
;;       (add-hook 'js-mode-hook #'jscs-indent-apply))
;;
;;     (with-eval-after-load 'js2-mode
;;       (add-hook 'js2-mode-hook #'jscs-indent-apply))

;;; Code:

(require 'json)

(defvar js-indent-level)
(defvar js2-basic-offset)

(defvar jscs-indent--presets-path
  (expand-file-name "../lib/node_modules/jscs/presets"
		    (file-name-directory (executable-find "jscs"))))

(defun jscs-indent--read-jscsrc ()
  (let ((dir (locate-dominating-file default-directory ".jscsrc")))
    (when dir
      (json-read-file (expand-file-name ".jscsrc" dir)))))

(defun jscs-indent--read-preset (name)
  (let ((preset (expand-file-name (concat name ".json")
				  jscs-indent--presets-path)))
    (if (file-readable-p preset)
	(json-read-file preset)
      (error "Preset %s is not found" name))))

(defun jscs-indent--config-list (config)
  (let ((preset (cdr (assq 'preset config))))
    (if (stringp preset)
	(cons config (jscs-indent--config-list
		      (jscs-indent--read-preset preset)))
      (list config))))

(defun jscs-indent--rule-validate-indentation (config)
  (let ((indent (cdr (assq 'validateIndentation config))))
    (prog1 indent
      (when (listp indent)
	(setq indent (cdr (assq 'value indent))))
      (cond
       ((integerp indent)
	(cond
	 ((memq major-mode '(js-mode json-mode)) (setq js-indent-level indent))
	 ((eq major-mode 'js2-mode) (setq js2-basic-offset indent)))
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
  (let ((config-list (jscs-indent--config-list config)))
    (dolist (func jscs-indent--rule-functions)
      (let ((tail config-list)
	    done)
	(while (and (not done) tail)
	  (setq done (funcall func (car tail)))
	  (setq tail (cdr tail)))))))

;;;###autoload
(defun jscs-indent-apply ()
  (interactive)
  (let ((jscsrc (jscs-indent--read-jscsrc)))
    (when jscsrc
      (jscs-indent--apply jscsrc))))

(provide 'jscs)
;;; jscs.el ends here
