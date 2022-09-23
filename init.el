;;
;; Author: Max Penet <m@qbits.cc>
;; URL: https://github.com/mpenet/emax
;; Keywords: emacs config

;;; Commentary:

;; Just my Emacs config

;;; License:

;; Copyright (C) 2021 Max Penet

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
      auth-sources '("~/.authinfo.gpg")
      warning-minimum-level :error
      gc-cons-threshold 50000000
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil
      large-file-warning-threshold 100000000
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
      frame-inhibit-implied-resize t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode ;; skip scratch
      mouse-yank-at-point t
      set-mark-command-repeat-pop t
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
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-j"))

;; (global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline-and-indent)
(global-set-key (kbd "<M-return>") 'comment-or-uncomment-region)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key (kbd "C-x '") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)
(global-set-key (kbd "<prior>") 'shrink-window)
(global-set-key (kbd "<next>") 'enlarge-window)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "M-j d") 'xref-find-definitions)
(global-set-key (kbd "M-j M-d") 'xref-find-definitions)
(global-set-key  (kbd "M-j r") 'xref-find-references)
(global-set-key (kbd "M-j M-r") 'xref-find-references)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono 10"))
;; (add-to-list 'default-frame-alist '(font . "JetBrainsMono 13"))
;; (add-to-list 'default-frame-alist '(font . "FiraCode-9"))

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height 100)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

;; utf8 only
(set-language-environment "UTF-8")


;; TAB => 4*'\b'
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default fill-column 80)

;; ui
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

(setq-default cursor-in-non-selected-windows nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; add the current column number to the mode bar
(column-number-mode t)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

;;; Packages
;;; via straight el

(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-built-in-pseudo-packages '(which-function-mode
                                          isearch
                                          dired
                                          js-mode
                                          project
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

(use-package diminish)

(use-package so-long
  :config (global-so-long-mode 1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; uniquify buffer names: append path if buffer names are identical
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package display-line-numbers
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(use-package visual-line-mode
  :hook ((org-mode . visual-line-mode)))

(use-package visual-fill-column
  :init (setq visual-fill-column-width 110
              visual-fill-column-center-text t)
  :hook ((org-mode . visual-fill-column-mode)))

(use-package which-function-mode
  :defer t
  :hook ((lisp-mode . which-function-mode)))

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode)
  :bind (("C-x C-u" . winner-undo)
         ("C-x u" . winner-undo)
         ("C-x C-j" . winner-redo)
         ("C-x j" . winner-redo)))

(use-package isearch
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        isearch-allow-scroll 'unlimited)
  :bind (:map isearch-mode-map
              ("C-c C-o" . isearch-occur)))

(use-package wgrep)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))
  :bind ((:map minibuffer-local-map
               ("C-c C-o" . embark-export)
               ("C-l" . embark-act))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  ;;(setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t
        completion-cycle-threshold 3

        ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
        ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
        read-extended-command-predicate
        #'command-completion-default-include-p

        ;; Enable indentation+completion using the TAB key.
        ;; `completion-at-point' is often bound to M-TAB.
        tab-always-indent 'complete))

(use-package xref
  :init (setq xref-prompt-for-identifier nil))

(use-package consult
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; Optionally configure a function which returns the project root directory
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  :bind (("C-t" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ;; ("C-x C-SPC" . consult-global-mark) ; just use default
         ("C-c C-SPC" . consult-global-mark)
         ("C-x C-g" . consult-git-grep)
         ("C-x C-i" . consult-imenu-multi)
         ("C-c C-i" . consult-imenu)))

(use-package consult-flycheck
  :after consult
  :config
  (setq flycheck-display-errors-delay 0.5)
  :bind (("C-x C-l" . consult-flycheck)
         ("C-x l" . consult-flycheck)))

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

(use-package all-the-icons)

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package project
  :init (setq project-ignores '("\\.clj-kondo" "\\.cpcache" "*\\.cp"))
  :bind (("C-x f" . project-find-file)))

(use-package whitespace
  :diminish
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook yaml-mode-hook))
    (add-hook hook #'whitespace-mode))

  (add-hook 'before-save-hook
            (lambda ()
              (if (member 'lsp-mode local-minor-modes)
                  (lsp-format-buffer)
                (whitespace-cleanup))))

  :config
  (setq whitespace-style '(face tabs empty trailing ;; lines-tail
                                )))

(use-package hl-todo
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
  (setq dired-dwim-target nil)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package diredfl
  :config (diredfl-global-mode 1))

(use-package magit
  :config
  ;; fix for https://github.com/magit/magit/issues/4766
  (defun prevent-whitespace-mode-for-magit ()
    (not (derived-mode-p 'magit-mode)))
  (add-function :before-while whitespace-enable-predicate 'prevent-whitespace-mode-for-magit)
  ;; :pin "melpa-stable"
  :bind (("C-x g" . magit-status)
         ("C-c C-g" . magit-status)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package gist)

(use-package yasnippet
  :diminish
  :config
  (yas-global-mode t)
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
        yas-indent-line nil)
  :diminish yas-minor-mode)

(use-package clojure-snippets)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("<C-return>" . corfu-insert))
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (setq  kind-icon-blend-frac 0.24)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eldoc
  :diminish)

(use-package expand-region
  :bind (("C-o" . er/expand-region)
         ("C-M-o" . er/contract-region)))

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
              ;; ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)
              ("M-r" . nil)
              ("C-j" . nil))
  :diminish)

(use-package clojure-mode
  :custom (cider-edit-jack-in-command t)
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :diminish
  :config
  (setq nrepl-log-messages t
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil
        cider-use-xref nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; use lsp
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (setq lsp-keymap-prefix "M-l")
  (defun mpenet/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex

  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (lsp-completion-mode . mpenet/lsp-mode-setup-completion))

  :bind (:map lsp-mode-map
              ("M-l M-l" . lsp-execute-code-action)
              ("M-j d" . lsp-find-definition)
              ("M-j M-d" . lsp-find-definition)
              ("M-j r" . lsp-find-references)
              ("M-j M-r" . lsp-find-references))

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq cljr-add-ns-to-blank-clj-files nil
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-auto-activate nil
        lsp-semantic-tokens-enable t
        ;; after last buffer closed, kill workspace
        lsp-keep-workspace-alive nil)

  :custom-face
  (lsp-face-semhl-namespace  ((t :inherit font-lock-type-face :weight normal)))
  (lsp-face-semhl-definition  ((t :inherit font-lock-function-name-face :weight normal))))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq ;; lsp-ui-peek-list-width 60
        ;; lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package consult-lsp)

(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :config
  (setq js-indent-level tab-width)
  (add-hook 'js-mode-hook 'yas-minor-mode))

(use-package fennel-mode
  :hook (fennel-mode . paredit-mode)
  :bind (:map fennel-mode-map
              ("C-c C-c" . lisp-eval-defun)))

(use-package inferior-lisp
  :hook (inferior-lisp-mode . paredit-mode))

(use-package lua-mode)

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  :diminish)

(use-package css-mode
  :config
  (progn (add-hook 'css-mode-hook 'rainbow-mode)
         (setq css-indent-offset tab-width)))

(use-package zencoding-mode
  :config
  (setq zencoding-preview-default nil)
  (add-hook 'sgml-mode-hook 'zencoding-mode))

(use-package htmlize
  :config (setq org-export-htmlize-output-type 'css))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package yaml-mode
  :defer t)

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package rst
  :bind (:map rst-mode-map
              ("C-M-h" . backward-kill-word)))

(use-package restclient
  :mode ("\\.http$". restclient-mode))

(use-package gist)

(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (set-face-attribute 'doom-themes-visual-bell nil
                      :background (doom-color 'base3)))

(use-package solaire-mode
  :config (solaire-global-mode +1))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
  :config
  (show-paren-mode +1)
  (set-face-foreground 'show-paren-match "red"))

(use-package emojify
  :config
  (setq emojify-display-style 'image)
  ;; only replace unicode and github, no ascii)
  (setq emojify-emoji-styles '(unicode github))
  ;; echo the actual underlying character to the minibuffer when point
  ;; is over them so we don't mess with the displayed buffer itself
  (setq emojify-point-entered-behaviour 'echo)
  (global-emojify-mode 1))

(use-package flycheck-pos-tip)

(use-package flycheck
  :bind (("C-c C-l" . flycheck-list-errors))
  :config
  (flycheck-pos-tip-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :bind (:map flyspell-mode-map
              ("<M-return>" . comment-or-uncomment-region))
  :diminish)

(use-package scratch)

(use-package dockerfile-mode)

(use-package eshell
  :after esh-mode
  :bind (:map eshell-mode-map
              ("C-r" . consult-history)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package sudo-edit)

(use-package org-modern
  :after org-roam
  :init (setq org-startup-indented t)
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.roam")
  (org-roam-completion-everywhere t)
  :bind (("M-r M-r" . org-roam-node-find)
         ("M-r l" . org-roam-buffer-toggle)
         ("M-r i" . org-roam-node-insert)
         ("M-r c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package org
  :init
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-babel-clojure-backend 'cider)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (js . t)
                                 (emacs-lisp . t)
                                 (clojure . t)
                                 (python . t))))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defun screenshot ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs-screenshot-" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (browse-url filename)
    (message filename)))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
