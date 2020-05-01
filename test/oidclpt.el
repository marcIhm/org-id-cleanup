;;; oidclpt.el --- Regression Tests for org-id-cleanup.el

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Marc Ihm <1@2484.de>
;; Keywords: outlines, regression-tests, elisp
;; Requires: org, org-id-cleanup
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Purpose:
;;
;;  Regression tests for package org-id-cleanup.el.
;;
;; Setup:
;;
;;  None required
;;
;;
;;
;; Further reading:
;;
;;  See org-id-cleanup.el, which is tested by this package
;;

;;; Code:

(require 'org-id-cleanup)
(require 'org-id)
(require 'cl-lib)
(require 'ert)
(require 'dash)

(defvar oidclpt-ert-work-file (concat temporary-file-directory "oidclpt-ert-work.org"))
(defvar oidclpt-attachment (concat temporary-file-directory "oidclpt-attachment"))
(defvar oidclpt-work-buffer nil)
(defvar oidclpt-ids '(""
		      "53e15dce-6f28-4674-bd65-e63b516d97ac"
		      "87512329-a204-47e5-b38c-1b22838b6f7d"
		      "b77473f3-dba0-4b4f-9db7-3ba095d12de4"
		      "2a3d87d0-9ad0-416b-aa22-dea96fede8b7"))

;;
;; All tests
;;

(ert-deftest oidclpt-test-aaa-test-test-setup ()
  (oidclpt-with-test-setup
    (message "Testing test setup")))


(ert-deftest oidclpt-test-assistant-from-start-to-end ()
  (oidclpt-with-test-setup
    (let (ids)
	(org-id-cleanup)
      (oidclpt-press-button "button")
      (oidclpt-press-button "go")
      (goto-char (point-min))
      (search-forward "--- start")
      (end-of-line)
      (setq buffer-read-only nil)
      (insert "\n")
      (insert oidclpt-ert-work-file) 
      (dotimes (_ 3)
	(oidclpt-press-button "continue"))
      (setq ids (oidclpt--collect-ids-from-list "--- List of"))
      (should (= (length ids) 1))
      (should (string= (nth 0 ids) (nth 3 oidclpt-ids)))
      (oidclpt-press-button "continue")
      (oidclpt-press-button "button")
      (oidclpt-press-button "go")
      (goto-char (point-min))
      (search-forward "Assistant done.")
      (with-current-buffer oidclpt-work-buffer
	(setq ids (oidclpt--collect-ids-from-properties)))
      (should (= (length ids) 3))
      (should (not (-difference (list "" (nth 3 oidclpt-ids))
				(-difference oidclpt-ids ids)))))))


;;
;; Helper functions
;;

(defmacro oidclpt-with-test-setup (&rest body)
  "Execute body within test setup"
  (declare (indent 0) (debug t))
  `(progn
     (oidclpt-setup-test)
     (unwind-protect
         (progn ,@body)
       (oidclpt-teardown-test))))


(defun oidclpt-setup-test ()
  (interactive)
  ;; remove any left over buffers
  (oidclpt-remove-work-buffers)
  ;; create them new
  (oidclpt-create-work-buffer)
  (switch-to-buffer oidclpt-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front oidclpt-ert-work-file)
  (switch-to-buffer oidclpt-work-buffer)
  (org-cycle '(64))
  (delete-other-windows)
  (end-of-buffer))


(defun oidclpt-teardown-test ()
  (interactive)
  (with-current-buffer oidclpt-work-buffer
    (set-buffer-modified-p nil)
    (basic-save-buffer))
  (org-remove-file oidclpt-ert-work-file))


(defun oidclpt-remove-work-buffers ()
  "Remove any left over work buffers"
  (let ((b (get-buffer "oidclpt-ert-work.org")))
    (when b
      (with-current-buffer b
	(set-buffer-modified-p nil))
      (kill-buffer b)))
  (setq oidclpt-work-buffer nil))


(defun oidclpt-press-button (text &optional)
  "Press the first button with this text"
  (let (found)
    (goto-char (point-min))
    (while (not found)
      (search-forward text)
      (backward-char)
      (when (overlays-at (point))
	(push-button)
	(setq found t)))))

;;
;; Test data
;;

(defun oidclpt-create-work-buffer ()
  (unless oidclpt-work-buffer
    (setq oidclpt-work-buffer (find-file-noselect oidclpt-ert-work-file)))
  (with-current-buffer oidclpt-work-buffer
    (setq buffer-save-without-query t)
    (auto-save-mode t)
    (if (file-exists-p buffer-auto-save-file-name)
        (delete-file buffer-auto-save-file-name))
    (erase-buffer)
    (insert
     (format "
* eins
  :PROPERTIES:
  :ID:       %s
  :END:

  This node has an attachment

* zwei
  :PROPERTIES:
  :ID:       %s
  :END:

* drei
  :PROPERTIES:
  :ID:       %s
  :END:

  Reference to zwei: %s

  This is the only node with an ID that should be deleted.

** vier           :ATTACH:
   :PROPERTIES:
   :ID:       %s
   :END:

   This node only has the attach property, but no attachment

"
	     (nth 1 oidclpt-ids)
	     (nth 2 oidclpt-ids)
	     (nth 3 oidclpt-ids)
	     (nth 2 oidclpt-ids)
	     (nth 4 oidclpt-ids)))
    
    (org-mode)
    ;; add attachment
    (goto-char (point-min))
    (search-forward "eins")
    (save-excursion
      (find-file oidclpt-attachment)
      (erase-buffer)
      (insert "Content of attachment\n")
      (basic-save-buffer))
    (org-attach-new oidclpt-attachment)))


(defun oidclpt--collect-ids-from-list (head)
  "Collect and return IDs from list at end of buffer."
  (let (ids)
    (goto-char (point-min))
    (search-forward head)
    (forward-line)
    (while (< (point) (point-max))
      (push (buffer-substring-no-properties (point) (point-at-eol)) ids)
      (forward-line))
    ids))


(defun oidclpt--collect-ids-from-properties ()
  "Collect and return IDs from properties."
  (let (ids)
    (goto-char (point-min))
    (while (search-forward ":ID:" nil t)
      (push (string-trim (buffer-substring-no-properties (point) (point-at-eol)))
	    ids))
    ids))


(provide 'oidclpt)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidclpt.el ends here
