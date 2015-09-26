;;; jscs-test.el --- Test for jscs.el                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  papaeye

;; Author: papaeye <papaeye@gmail.com>
;; Keywords: languages, convenience

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

;;

;;; Code:

(defconst jscs-test-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar jscs-command)
(setq jscs-command (expand-file-name "node_modules/.bin/jscs"
				     jscs-test-directory))

(require 'package)
(setq package-user-dir
      (expand-file-name (concat "../.cask/" emacs-version "/elpa")
			jscs-test-directory))
(package-initialize)

(eval-when-compile (require 'cl-lib))
(require 'ert)
(require 'jscs)

(defconst jscs-test-cases-directory
  (expand-file-name "cases" jscs-test-directory))

(defun jscs-test-case-directory (case)
  (expand-file-name case jscs-test-cases-directory))

(defun jscs-test-last-message ()
  (with-current-buffer "*Messages*"
    (save-excursion
      (let ((end (goto-char (1- (point-max))))
	    (beg (1+ (search-backward "\n"))))
	(buffer-substring-no-properties beg end)))))

(defmacro jscs-fix-deftest (case expected-message &optional display-errors)
  (let* ((case-name (format "fix/%s" case))
	 (test-name (format "jscs/%s" case-name)))
    `(ert-deftest ,(intern test-name) ()
       (let* ((default-directory (jscs-test-case-directory ,case-name))
	      (test-file-name (expand-file-name "main.js" default-directory))
	      actual-message)
	 (with-temp-buffer
	   (insert-file-contents-literally test-file-name)
	   (set-visited-file-name test-file-name)
	   (cl-letf (((symbol-function 'message)
		      (lambda (format-string &rest args)
			(setq actual-message (format format-string args)))))
	     (jscs-fix))
	   (should (string= actual-message ,expected-message))
	   (when ,display-errors
	     (should (get-buffer "*Jscs-Fix Errors*"))))))))

(jscs-fix-deftest failed "Could not apply jscs-fix" t)
(jscs-fix-deftest fatal-error "Could not apply jscs-fix" t)
(jscs-fix-deftest fixed "Applied jscs-fix")
(jscs-fix-deftest fixed-partially "Applied jscs-fix partially" t)
(jscs-fix-deftest missing-config "No configuration found" t)
(jscs-fix-deftest no-errors "Buffer is already jscs-fixed")

(provide 'jscs-test)
;;; jscs-test.el ends here
