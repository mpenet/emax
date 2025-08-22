;;  -*- lexical-binding: t; -*-
;; Author: Max Penet <mpenet@s-exp.com>
;; URL: https://github.com/mpenet/emax
;; Keywords: emacs config
;;; Commentary:

;; Just my Emacs config

;;; License:

;; Copyright (C) 2025 Max Penet

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

;;; bootstrap via straight el
(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-recipes-gnu-elpa-use-mirror t
      straight-built-in-pseudo-packages '(which-function-mode
                                          isearch
                                          dired
                                          bookmark
                                          vc
                                          ;; eglot
                                          use-package
                                          org
                                          project
                                          js-mode
                                          uniquify
                                          inferior-lisp
                                          visual-line-mode))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Packages


(use-package emacs
  :custom
  (epg-gpg-program "/opt/homebrew/bin/gpg")
  (epg-gpg-home-directory "~/.gnupg/")
  (auth-sources '("~/.authinfo"))
  (load-prefer-newer t)
  (warning-minimum-level :error)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (auto-window-vscroll nil)
  (large-file-warning-threshold 100000000)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (use-short-answers t)
  (kill-ring-max 150)
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "open")
  (truncate-partial-width-windows nil)
  (backup-inhibited t)
  (make-backup-files nil)
  (auto-save-default nil)
  (auto-save-list-file-prefix nil)
  (save-place nil)
  (vc-follow-symlinks nil)
  (inhibit-startup-message t)
  (frame-inhibit-implied-resize t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)  ;; skip scratch
  (mouse-yank-at-point t)
  ;; (which-function-mode t)
  (set-mark-command-repeat-pop t)
  (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key. `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (hippie-expand-try-functions-list
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
  
  (indent-tabs-mode nil)
  (tab-width 4)
  (fill-column 80)

  ;; add the current column number to the mode bar
  (column-number-mode t)
  ;; typed text replaces the selection if the selection is active
  (delete-selection-mode t)
  (set-language-environment "UTF-8")
  (cursor-in-non-selected-windows nil)
  ;; case insensitive searches
  (case-fold-search t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  ;; Emacs 28+: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (inferior-lisp-program "/opt/homebrew/bin/sbcl")


  :bind
  (("M-j" . nil)
   ("M-r" . nil)
   ("M-c" . nil)   
   ("C-x m" . execute-extended-command)
   ("C-x C-m" . execute-extended-command)
   ("C-M-h" . backward-kill-word)
   ("C-M-<backspace>" . backward-kill-word)
   ("RET" . newline-and-indent)
   ("<C-return>" . newline-and-indent)
   ("<M-return>" . comment-or-uncomment-region)
   ("C-x C-o" . other-window)
   ("C-x C-k" . kill-buffer)
   ("C-x . " . delete-other-windows)
   ("C-x ," . split-window-below)
   ("C-x ." . split-window-right)
   ("C-x l" . delete-window)
   ("C-x C-r" . query-replace)
   ("M-i" . hippie-expand)
   ("M-i" . hippie-expand)
   ("M-j d" . xref-find-definitions)
   ("M-j M-d" . xref-find-definitions)
   ("M-j r" . xref-find-references)
   ("M-j M-r" . xref-find-references)
   ("C-<next>" . text-scale-increase)
   ("M-<prior>" . text-scale-decrease)
   ("M-j M-r" . xref-find-references))
  :init
  (set-face-attribute 'default nil
                      :font "PragmataPro Mono Liga"
                      :weight 'normal
                      :height (let ((w (x-display-pixel-width)))
                                (cond
                                 ((= w 4072) 220) ; exo
                                 ((>= w 3456) 180) ; mbp screen
                                 ((>= w 1920) 160) ; plugged
                                 (t 180)))) ;; default

  ;; full screen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(undecorated . t))

  ;; Do not allow the cursor in the minibuffer prompt
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defun er-auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))

  (add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

  (defun save-buffer-as-is ()
    "Save file \"as is\", that is in read-only-mode. Useful when you
want to avoid having the hooks run"
    (interactive)
    (if buffer-read-only
        (save-buffer)
      (read-only-mode 1)
      (save-buffer)
      (read-only-mode 0)))

  ;; config changes made through the customize UI will be stored here
  (when (file-exists-p custom-file)
    (load custom-file))
  ;; :config
  ;; (let ((personal-file "~/.personal.el.gpg"))
  ;;   (when (file-exists-p personal-file)
  ;;     (load-library personal-file)))
  )

(use-package diminish)

(use-package ligature-pragmatapro
  :config
  (ligature-pragmatapro-setup)
  (global-ligature-mode))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; uniquify buffer names: append path if buffer names are identical
(use-package uniquify
  :custom (uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package display-line-numbers
  :hook ((prog-mode conf-mode yaml-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(use-package visual-line-mode
  :hook ((org-mode . visual-line-mode)))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t)
  :hook ((org-mode . visual-fill-column-mode)))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode +1)
  :bind (("C-x C-u" . winner-undo)
         ("C-x u" . winner-undo)))

(use-package ws-butler
  :diminish
  :straight (:host github
             :branch "master"
             :repo "lewang/ws-butler")
  :hook ((prog-mode . ws-butler-mode)))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (isearch-allow-scroll 'unlimited)
  :bind (:map isearch-mode-map
              ("C-c C-o" . isearch-occur)
              ("M-s o" . isearch-occur)))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 3)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode +1))

(use-package wgrep)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  :bind ((:map minibuffer-local-map
               ("C-c C-o" . embark-export)
               ("C-l" . embark-act))))

(use-package xref
  :custom (xref-prompt-for-identifier nil))

(use-package consult
  :preface
  (defun mpenet/consult-flymake-project ()
    (interactive)
    (consult-flymake t))
  :custom 
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :bind (("C-t" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-g" . consult-git-grep)
         ("C-x C-i" . consult-imenu-multi)
         ("C-c C-i" . consult-imenu)
         ("M-g M-g" . consult-goto-line)
         ("M-g f" . mpenet/consult-flymake-project)
         ("C-x C-SPC" . consult-global-mark)))

(use-package embark
  :config
  (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  (add-hook 'embark-collect-post-revert-hook
            (defun resize-embark-collect-window (&rest _)
              (when (memq embark-collect--kind '(:live :completions))
                (fit-window-to-buffer (get-buffer-window)
                                      (floor (frame-height) 2) 1)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :straight (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :straight (nerd-icons-ibuffer :type git :host github :repo "seagle0128/nerd-icons-ibuffer")
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package project
  :custom
  (project-ignores '("\\.clj-kondo" "\\.cpcache" "*\\.cp"))
  (project-switch-commands #'project-find-file)
  ;; NEW
  ;; (project-mode-line t)
  :bind (("C-x f" . project-find-file)))

(use-package hl-todo
  :custom (hl-todo-highlight-punctuation ":")
  :config (global-hl-todo-mode))

(use-package solaire-mode
  :config (solaire-global-mode))

(use-package dired
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (dired-dwim-target nil)
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package diredfl
  :config (diredfl-global-mode 1))


(use-package magit
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :bind (("C-c C-g" . magit-status)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package gist)

(use-package expand-region
  :bind (("C-o" . er/expand-region)
         ("C-M-o" . er/contract-region)))

(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil)
  :straight (eldoc :source gnu-elpa-mirror)
  :diminish)

(use-package bookmark
  :config
  (defun mpenet--bookmark-set ()
    (interactive)
    (bookmark-set-internal nil
                           (concat (buffer-name) " "
                                   (which-function) " "
                                   (number-to-string (line-number-at-pos)) ":"
                                   (number-to-string (current-column)))
                           'overwrite))
  :bind (("C-x r r" . mpenet--bookmark-set)
         ("C-x r d" . bookmark-delete)))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (defun my-paredit-delete ()
    "If a region is active check if it is balanced and delete it otherwise
        fallback to regular paredit behavior"
    (interactive)
    (if mark-active
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete)))
  ;; (define-key paredit-mode-map (kbd "C-j") nil)
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-backward-kill-word)
              ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)
              ("RET" . nil)
              ("M-r" . nil)
              ("C-j" . nil)
              ("C-M-j"))
  :diminish)

(use-package clojure-mode
  :custom (cider-edit-jack-in-command t)
  :init (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
  :mode "\\.bb\\'"
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package cider
  :diminish
  :custom
  ;; cider-font-lock-dynamically nil ; use lsp semantic tokens
  (nrepl-log-messages t)
  (cider-eldoc-display-for-symbol-at-point nil) ; use lsp
  (cider-prompt-for-symbol nil)
  (cider-use-xref nil)
  :custom-face (cider-debug-code-overlay-face
                ((t (:box (:line-width -1 :color "orange")))))
  :config
  (add-hook 'cider-repl-mode-hook #'paredit-mode)

  (defun mpenet/eglot-completion-at-point ()
    (when (boundp 'eglot-completion-at-point)
      (funcall 'eglot-completion-at-point)))

  (defun mpenet/cider-completion-at-point ()
    (funcall 'cider-complete-at-point))

  (defun mpenet/cider-capf ()
    (setq-local completion-at-point-functions
                (list #'mpenet/eglot-completion-at-point
                      #'mpenet/cider-completion-at-point)))
 
  :hook ((cider-mode . mpenet/cider-capf)
         (cider-repl-mode . mpenet/cider-capf)
         (cider-disconnected . mpenet/cider-capf))
  :bind (:map
         cider-mode-map
         ("C-c C-d" . cider-debug-defun-at-point)
         ("C-c d" . cider-debug-defun-at-point)
         :map
         cider-repl-mode-map
         ("C-j" . nil)))

;;; eglot

(use-package eglot
  :straight (eglot :source gnu-elpa-mirror)
  :ensure t
  :commands (eglot eglot-ensure)
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit 'highlight :background "#434C5E"))))
  (eglot-code-action-indicator-face ((t (:inherit default
                                                  :weight normal
                                                  :foreground "#f5d742"
                                                  :background "#161618"))))
  :custom
  (left-margin-width 1)
  (eglot-code-action-indicator "")
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0))
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (before-save . (lambda ()
                          (when (eglot-managed-p)
                            (eglot-format-buffer)))))

  :bind (:map eglot-mode-map
              ("M-l M-l" . eglot-code-actions))
  :config
  (diminish 'eldoc-mode)
  (advice-add 'eglot--format-markup
              :around (lambda (orig &rest args) (let ((inhibit-read-only t)) (apply orig args))))
  (fset #'jsonrpc--log-event #'ignore))

(use-package jarchive
  :diminish
  :straight (jarchive :type git
                      :host nil
                      :repo "https://git.sr.ht/~dannyfreeman/jarchive")
  :hook ((clojure-mode . jarchive-setup)
         (clojurec-mode . jarchive-setup)))

(use-package eglot-booster
    :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
	:after eglot
	:config	(eglot-booster-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.3))
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)          ;; pop after 1 char
  (corfu-auto-delay 0.1)         ;; pop after delay 0.1s
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("<C-return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package flymake
  :bind ("C-x p l" . flymake-show-project-diagnostics)
  :custom (flymake-mode-line-lighter ""))

(use-package sideline
  :diminish
  :preface (setq sideline-custom-default-face '((t :height 0.8 :box nil :slant italic)))
  :hook (flymake-mode . sideline-mode)
  :custom-face
  (sideline-default ,sideline-custom-default-face)
  (sideline-backend,sideline-custom-default-face)
  (sideline-flymake-error ,sideline-custom-default-face)
  (sideline-flymake-warning ,sideline-custom-default-face)
  (sideline-flymake-success ,sideline-custom-default-face)
  :hook ((flymake-mode  . sideline-mode)))

(use-package sideline-flymake
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-backends-right '(sideline-flymake)))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :custom
  (js-indent-level tab-width))

(use-package fennel-mode
  :custom (fennel-program "fnl --repl")
  :hook (fennel-mode . paredit-mode)
  :bind (:map fennel-mode-map
              ("C-c C-c" . lisp-eval-defun)))
  
(use-package groovy-mode)

(use-package inferior-lisp
  :hook (inferior-lisp-mode . paredit-mode))

(use-package lua-mode)

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  :diminish)

(use-package css-mode
  :custom (css-indent-offset tab-width)
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package zencoding-mode
  :custom (zencoding-preview-default nil)
  :config
  (add-hook 'sgml-mode-hook 'zencoding-mode))

(use-package htmlize
  :custom (org-export-htmlize-output-type 'css))

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "marked")
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package yaml-mode)

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package rst
  :bind (:map rst-mode-map
              ("C-M-h" . backward-kill-word)))

(use-package restclient
  :mode ("\\.http$". restclient-mode))

(use-package gist)

(use-package yasnippet
  :custom
  (yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt))
  (yas-indent-line nil)
  :config
  (yas-global-mode t)
  :diminish yas-minor-mode)

(use-package clojure-snippets)

(use-package breadcrumb
  :config (breadcrumb-mode))

(use-package hl-line
  :config (global-hl-line-mode t))

(use-package kaolin-themes
  :straight (kaolin-themes :type git :host github :repo "mpenet/emacs-kaolin-themes" :branch "mpenet/devel")
  :custom
  (kaolin-themes-modeline-border nil)
  (kaolin-themes-italic-comments t)
  (kaolin-mono-dark-alt-bg t)
  :config
  (load-theme 'kaolin-mono-dark t))

(use-package padded-modeline
  :straight (padded-modeline :type git
                             :host github
                             :repo "mpenet/padded-modeline"
                             :branch "main")
  :config
  (padded-modeline-mode t))

(use-package symbol-overlay
  :custom-face
  (symbol-overlay-face-1 ((t (:background "dodger blue" :foreground "black"))))
  (symbol-overlay-face-2 ((t (:background "hot pink" :foreground "black"))))
  (symbol-overlay-face-3 ((t (:background "yellow" :foreground "black"))))
  (symbol-overlay-face-4 ((t (:background "orchid" :foreground "black"))))
  (symbol-overlay-face-5 ((t (:background "red" :foreground "black"))))
  (symbol-overlay-face-6 ((t (:background "salmon" :foreground "black"))))
  (symbol-overlay-face-7 ((t (:background "spring green" :foreground "black"))))
  (symbol-overlay-face-8 ((t (:background "turquoise" :foreground "black"))))
  :bind (("M-o" . nil)
         ("M-o o". symbol-overlay-put)
         ("M-o M-o". symbol-overlay-put)
         ("M-o r" . symbol-overlay-remove-all)
         ("M-o M-r" . symbol-overlay-remove-all)         
         ("M-o s" . symbol-overlay-toggle-in-scope)
         ("M-o M-s" . symbol-overlay-toggle-in-scope)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
  :config
  (show-paren-mode +1)
  (set-face-foreground 'show-paren-match "red"))

(use-package emojify
  :custom
  (emojify-display-style 'image)
  ;; only replace unicode and github, no ascii)
  (emojify-emoji-styles '(unicode github))
  ;; echo the actual underlying character to the minibuffer when point
  ;; is over them so we don't mess with the displayed buffer itself
  (emojify-point-entered-behaviour 'echo)
  :config
  (global-emojify-mode 1))

(use-package jinx
  :disabled
  :diminish
  :hook ((text-mode prog-mode conf-mode) . jinx-mode)
  :bind (("M-j c" . jinx-correct)
         ("M-j l" . jinx-languages)))

(use-package scratch)

(use-package dockerfile-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package sudo-edit)

(use-package org-modern
  :custom (org-startup-indented t)
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org
  :custom
  (org-babel-clojure-backend 'cider)
  (org-src-tab-acts-natively t)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (js . t)
                                 (emacs-lisp . t)
                                 (clojure . t)
                                 (python . t))))

(use-package sly)

(use-package sly-overlay)

(use-package popper
  :custom (popper-window-height 25)
  :bind (("C-j" . popper-toggle)
         ("C-M-j" . popper-cycle)
         ("C-M-t" . popper-toggle-type))
  :init
  (setq popper-reference-buffers '("\\*Messages\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   eca-chat-mode
                                   sly-mrepl-mode
                                   cider-repl-mode
                                   help-mode
                                   compilation-mode)
        popper-group-function #'popper-group-by-project
        popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
        popper-echo-dispatch-actions t)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package gptel
  :custom
  (gptel-include-reasoning nil)
  (gptel-log-level 'debug)
  :bind (("C-x g" . gptel))
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package gptel-prompts
  :straight (:host github :repo "jwiegley/gptel-prompts")
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))


(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick")
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

(use-package copilot
  :config
  (defun mpenet/copilot-complete-or-accept ()
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion))
      (copilot-complete)))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (("C-M-c" . mpenet/copilot-complete-or-accept)
         (:map copilot-mode-map
               ("C-c C-p" . copilot-previous-completion)
               ("C-c C-n" . copilot-next-completion)
               ("C-c g" . copilot-clear-overlay))))

(use-package rustic
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package reformatter)

(use-package go-ts-mode
  :mode ("\\.go$")
  :hook
  (go-ts-mode . go-format-on-save-mode)
  (go-ts-mode . copilot-mode)
  :init
  (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))

  :config
  (reformatter-define go-format
    :program "goimports"
    :args '("/dev/stdin")))

(use-package java-ts-mode
  :init
  (add-to-list 'treesit-language-source-alist '(java "https://github.com/tree-sitter/tree-sitter-java"))
  :mode ("\\.java$"))

(use-package fish-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package protobuf-mode)

(use-package earthfile-mode)

(use-package eca
  :straight (:host github
                   :branch "master"
                   :repo "editor-code-assistant/eca-emacs"))
