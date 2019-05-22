;;; init.el --- @mpenet Emacs config
;;
;; Author: Max Penet <m@qbits.cc>
;; URL: https://github.com/mpenet/emax
;; Keywords: emacs config

;;; Commentary:

;; Just my Emacs config

;;; License:

;; Copyright (C) 2019  Max Penet

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:


(setq load-prefer-newer t
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000
      tramp-default-method "ssh"
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome"
      truncate-partial-width-windows nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      save-place nil
      vc-follow-symlinks nil
      inhibit-startup-message t
      initial-scratch-message nil
      visible-bell t
      hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
                               try-expand-dabbrev-all-buffers
                               try-expand-dabbrev-from-kill
                               try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-all-abbrevs
                               try-expand-list
                               try-expand-line
                               try-complete-lisp-symbol-partially
                               try-complete-lisp-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)
(global-set-key (kbd "<f4>")
                (lambda ()
                  (interactive)
                  (if defining-kbd-macro
                      (end-kbd-macro)
                    (start-kbd-macro nil))))
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key (kbd "C-x '") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)
(global-set-key (kbd "C-.") 'find-tag)
(global-set-key (kbd "C-,") 'pop-tag-mark)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "C-t") 'hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-font "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-1")

;; utf8 only
(setq current-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; TAB => 4*'\b'
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default sgml-basic-offset tab-width)

;; ui
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

;; add the current line number to the mode bar
(line-number-mode t)

;; add the current column number to the mode bar
(column-number-mode t)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

;; make emacs use the clipboard if running in X
(when window-system
  (setq select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/"))
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-enable-at-startup nil)

(unless package--initialized (package-initialize t))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; uniquify buffer names: append path if buffer names are identical
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package display-line-numbers
  :if (version<= "26" emacs-version)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(use-package which-function-mode
  :defer t
  :hook ((prog-mode . which-function-mode)))

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode)
  :bind (("C-x C-u" . winner-undo)
         ("C-x u" . winner-undo)
         ("C-x C-j" . winner-redo)
         ("C-x j" . winner-redo)))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-c b" . ivy-switch-buffer)
         ("C-j" . ivy-immediate-done))
  :init
  (ivy-mode 1)
  :config
  (setq-default ;; ivy-use-virtual-buffers t
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-virtual-abbreviate 'fullpath
   ivy-count-format ""
   ivy-magic-tilde nil
   ivy-dynamic-exhibit-delay-ms 150
   ivy-re-builders-alist '((swiper . regexp-quote)
                           (counsel-M-x . ivy--regex-fuzzy)
                           (counsel-git . ivy--regex-fuzzy)
                           (t . ivy--regex-plus))))

(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x m" . counsel-M-x)
         ("C-x C-g" . counsel-rg)
         ("C-x f" . counsel-git)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-extra-directories nil))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face trailing lines-tail)
        whitespace-global-modes '(not erc-mode)
        whitespace-line-column 80))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package magit
  :pin "melpa-stable"
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g" . magit-status)))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
        yas-indent-line nil)
  :diminish yas-minor-mode)

(use-package company-quickhelp
  :ensure t)

(use-package company
  :pin "melpa-stable"
  :ensure
  :init
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-require-match nil
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
	                        company-preview-frontend
	                        company-echo-metadata-frontend))
  :config
  (company-quickhelp-mode 1)
  (global-company-mode)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("<tab>" . company-complete-selection)))

(use-package paredit
  :pin "melpa-stable"
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'erlang-mode-hook #'paredit-mode)
  :config
  (defun my-paredit-delete ()
    "If a region is active check if it is balanced and delete it otherwise
        fallback to regular paredit behavior"
    (interactive)
    (if mark-active
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete)))
  (define-key paredit-mode-map (kbd "C-j") nil)
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-backward-kill-word)
              ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :pin "melpa-stable"
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package flycheck-joker
  :ensure t)

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-mode
  :ensure t
  :requires (go-autocomplete go-gopath)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (flycheck-mode)
                            (set (make-local-variable 'company-backends)
                                 '(company-go))))

  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind
  (:map go-mode-map
        ("C-c C-e" . go-gopath-set-gopath)))

(use-package go-autocomplete
  :ensure t)

(use-package go-gopath
  :ensure t)

(use-package go-snippets
  :ensure t)

(use-package go-eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :config
  (setq js-indent-level tab-width)
  (add-hook 'js-mode-hook 'yas-minor-mode))

(use-package lua-mode
  :ensure t)

(use-package fennel-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package powerline
  :pin "melpa-stable"
  :ensure t)

(use-package rainbow-mode
  :defer t
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (progn (add-hook 'css-mode-hook 'rainbow-mode)
         (setq css-indent-offset tab-width)))

(use-package zencoding-mode
  :ensure t
  :config
  (setq zencoding-preview-default nil)
  (add-hook 'sgml-mode-hook 'zencoding-mode))

(use-package org
  :defer t
  :config
  (setq org-log-done 'time)
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas-trigger-key)
              (setq yas-trigger-key [tab])
              (add-to-list 'org-tab-first-hook
                           (lambda ()
                             (let ((yas-fallback-behavior 'return-nil))
                               (yas-expand))))
              (define-key yas-keymap [tab] 'yas-next-field))))

(use-package htmlize
  :ensure t
  :config (setq org-export-htmlize-output-type 'css))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package rst
  :bind (:map rst-mode-map
              ("C-M-h" . backward-kill-word)))

(use-package restclient
  :ensure t
  :mode ("\\.http$". restclient-mode))

(use-package gist
  :pin "melpa-stable"
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package color-theme-sanityinc-tomorrow
  :init
  (powerline-default-theme)
  (load-theme 'sanityinc-tomorrow-night t)
  :ensure t)

(use-package flycheck-dialyzer
  :ensure t)

(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (flycheck-pos-tip-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package erlang
  :ensure t
  :requires (flycheck-dializer paredit)
  :config
  (add-hook 'erlang-mode-hook 'flycheck-mode)
  (flycheck-add-next-checker 'erlang 'erlang-dialyzer)
  (add-hook 'erlang-mode-hook 'mpenet/disable-paredit-spaces-before-paren)
  (defun mpenet/disable-paredit-spaces-before-paren ()
    ;; Function which always returns nil -> never insert a space
    ;; when insert a parentheses.
    (defun mpenet/erlang-paredit-space-for-delimiter-p (endp delimiter) nil)
    ;; Set this function locally as only predicate to check when
    ;; determining if a space should be inserted before a newly
    ;; created pair of parentheses.
    (setq-local paredit-space-for-delimiter-predicates
                '(mpenet/erlang-paredit-space-for-delimiter-p))))

(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package erc-hl-nicks
  :ensure t)

(use-package es-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package clojure-snippets
  :ensure t)

(use-package erc
  :defer t
  :requires (erc-services)
  :init
  (defun erc-connect/freenode ()
    "Connect to ERC, or switch to last active buffer"
    (interactive)
    (erc :server "irc.freenode.net"
         :port 6667
         :nick "mpenet"
         :full-name "mpenet"))
  (setq erc-modules '(netsplit fill track completion ring button autojoin
                               services match stamp track page scrolltobottom
                               hl-nicks)
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-ignore-list '("chord" "chare")
        erc-autojoin-mode t
        erc-timestamp-format "%H:%M "
        erc-interpret-mirc-color t
        erc-input-line-position -2
        erc-prompt ">"
        erc-keywords '("\\bcassandra\\b" "\\balia\\b" "\\bhayt\\b")
        erc-insert-timestaamp-function 'erc-insert-timestamp-left
        ;; erc-current-nick-highlight-type 'nick
        erc-prompt-for-nickserv-password nil
        erc-autojoin-channels-alist
        '(("freenode.net" "#clojure" "#erlang" "#fennel" "#lua")))
  :config
  (erc-services-mode 1)
  (erc-track-mode t)
  :bind ("C-c e f" . erc-connect/freenode))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(put 'set-goal-column 'disabled nil)
