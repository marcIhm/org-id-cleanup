;;; oidclpt.el --- Regression Tests for org-id-cleanup.el

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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

(defvar oidclpt-work-buffer nil)

;;
;; All tests
;;

(ert-deftest oidclpt-test-aaa-test-test-setup ()
  (oidclpt-with-test-setup
    (message "Testing test setup")))


(ert-deftest oidclpt-test-clock-into-working-set ()
  (oidclpt-with-test-setup
    (unwind-protect
	(progn
	  (let ((org-id-cleanup-clock-in nil))
	    (should (not (org-clock-is-active)))
	    (oidclpt-goto "eins")
	    (oidclpt-do "s <down>")
	    (sleep-for 1)
	    (should (not (org-clock-is-active)))
	    
	    (setq org-id-cleanup-clock-in t)
	    (oidclpt-goto "zwei")
	    (oidclpt-do "s <down>")
	    (sleep-for 1)
	    (should (org-clock-is-active))))
      (org-clock-out))))


(ert-deftest oidclpt-test-assistant ()
  (oidclpt-with-test-setup
    (setq org-id-cleanup-id nil)
    (oidclpt-do "y e s <return> a")
    (should org-id-cleanup-id)
    (should (string= org-id-cleanup-id (car org-id-cleanup--ids)))))


(ert-deftest oidclpt-test-working-set-restore ()
  (oidclpt-with-test-setup
    (oidclpt-goto "zwei")
    (oidclpt-do "s")
    (should (= (length org-id-cleanup--ids) 1))
    (oidclpt-do "d")
    (should (= (length org-id-cleanup--ids) 0))
    (oidclpt-do "u")
    (should (= (length org-id-cleanup--ids) 1))))


(ert-deftest oidclpt-test-working-set-bottom-head ()
  (oidclpt-with-test-setup
    (oidclpt-goto "drei")
    (oidclpt-do "s")
    (beginning-of-buffer)
    (oidclpt-do "SPC b")
    (forward-line)
    (should (looking-at ".* vier"))
    (beginning-of-buffer)
    (oidclpt-do "SPC h")
    (should (looking-at ".* drei"))))


(ert-deftest oidclpt-test-working-set-menu-goto ()
  (oidclpt-with-test-setup
    (oidclpt-goto "zwei")
    (oidclpt-do "s")
    (oidclpt-goto "eins")
    (oidclpt-do "a")
    (oidclpt-do "m <down> <return>")
    (should (looking-at ".* zwei"))))


(ert-deftest oidclpt-test-working-set-menu-delete ()
  (oidclpt-with-test-setup
    (oidclpt-goto "zwei")
    (oidclpt-do "s")
    (oidclpt-goto "eins")
    (oidclpt-do "a")
    (should (= (length org-id-cleanup--ids) 2))
    (oidclpt-do "m <down> d q")
    (should (= (length org-id-cleanup--ids) 1))))


(ert-deftest oidclpt-test-double-working-set ()
  (oidclpt-with-test-setup
    (oidclpt-goto "zwei")
    (oidclpt-do "s")
    (oidclpt-goto "eins")
    (oidclpt-do "a")
    (oidclpt-do "SPC SPC")
    (should (looking-at ".* zwei"))
    (oidclpt-do "SPC")
    (should (looking-at ".* zwei"))
    (oidclpt-do "SPC SPC")
    (should (looking-at ".* eins"))))


(ert-deftest oidclpt-test-nested-working-set ()
  (oidclpt-with-test-setup
    (oidclpt-goto "drei")
    (oidclpt-do "s")
    (oidclpt-goto "vier")
    (oidclpt-do "a")
    (should (= (length org-id-cleanup--ids) 1))))


(ert-deftest oidclpt-test-log-of-working-set ()
  (oidclpt-with-test-setup
   (oidclpt-goto "zwei")
   (oidclpt-do "a")
   (oidclpt-goto "eins")
   (org-end-of-meta-data t)
   (should (looking-at "[[:blank:]]+-"))))


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


(defun oidclpt-do (keys &optional prefix)
  (execute-kbd-macro (kbd (concat prefix (if prefix " " "") "M-x o r g - w o r k i n g - s e t <return> " keys))))


(defun oidclpt-setup-test ()
  (interactive)
  ;; remove any left over buffers
  (oidclpt-remove-work-buffers)
  ;; create them new
  (oidclpt-create-work-buffer)
  (switch-to-buffer oidclpt-work-buffer)
  (basic-save-buffer)
  (org-agenda-file-to-front oidclpt-ert-work-file)
  (oidclpt-create-work-buffer)
  (switch-to-buffer oidclpt-work-buffer)
  (org-cycle '(64))
  (delete-other-windows)
  (end-of-buffer)
  (setq org-id-cleanup--ids nil)
  (setq org-id-cleanup--ids-do-not-clock nil))


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


(defun oidclpt-goto (name)
  (org-id-goto (cdr (assoc name oidclpt-names-ids))))

;;
;; Test data
;;


(defvar oidclpt-names-ids
  (list (cons "eins" "53e15dce-6f28-4674-bd65-e63b516d97ac")
	(cons "zwei" "87512329-a204-47e5-b38c-1b22838b6f7d")
	(cons "drei" "b77473f3-dba0-4b4f-9db7-3ba095d12de4")
	(cons "vier" "2a3d87d0-9ad0-416b-aa22-dea96fede8b7"))
  "Associating names of nodes with ids")


(defun oidclpt-create-work-buffer ()
  (unless oidclpt-work-buffer
    (setq oidclpt-work-buffer (find-file-noselect oidclpt-ert-work-file)))
  (with-current-buffer oidclpt-work-buffer
    (setq buffer-save-without-query t)
    (auto-save-mode t)
    (if (file-exists-p buffer-auto-save-file-name)
        (delete-file buffer-auto-save-file-name))
    (erase-buffer)
    (insert "
* eins
  :PROPERTIES:
  :ID:       53e15dce-6f28-4674-bd65-e63b516d97ac
  :END:
* zwei
  :PROPERTIES:
  :ID:       87512329-a204-47e5-b38c-1b22838b6f7d
  :END:
* drei
  :PROPERTIES:
  :ID:       b77473f3-dba0-4b4f-9db7-3ba095d12de4
  :END:
** vier
   :PROPERTIES:
   :ID:       2a3d87d0-9ad0-416b-aa22-dea96fede8b7
   :END:
")
    (org-mode)
    (setq org-id-cleanup-id "53e15dce-6f28-4674-bd65-e63b516d97ac")
    oidclpt-work-buffer))


(provide 'oidclpt)

;; Local Variables:
;; fill-column: 75
;; comment-column: 50
;; End:

;;; oidclpt.el ends here
