(setq load-prefer-newer t)
(add-to-list 'load-path "c:/org-id-cleanup")
(add-to-list 'load-path "c:/org-id-cleanup/test")

(setq package-user-dir "c:/org/elpa")
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq url-proxy-services nil)

(require 'org)
(require 'oidclpt)
(require 'paredit)
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))

(setq org-id-locations-file "c:/org-id-cleanup/org-id-locations-for-test")

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode nil)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(set-variable 'make-backup-files nil)
(setq ring-bell-function 'ignore)
(global-font-lock-mode 1)
(toggle-truncate-lines)
(setq initial-frame-alist '((width . 122) (height . 42) (top . 20) (left . 50) (name . "emacs")))

