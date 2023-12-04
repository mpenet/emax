;;
;; Author: Max Penet <m@qbits.cc>
;; URL: https://github.com/mpenet/emax
;; Keywords: emacs config
;;; Commentary:

;; Just my Emacs config

;;; License:

;; Copyright (C) 2023 Max Penet

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
                                          ;; eglot
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
  (load-prefer-newer t)
  (warning-minimum-level :error)
  (gc-cons-threshold 50000000)
  (read-process-output-max (* 1024 1024))
  (auto-window-vscroll nil)
  (large-file-warning-threshold 100000000)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
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
  (which-function-mode t)
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
  (pixel-scroll-precision-mode t)
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
  :custom-face (eglot-highlight-symbol-face ((t (:inherit 'highlight :background "#434C5E"))))
  :init
  (set-face-attribute 'default nil
                      :font "PragmataPro Mono Liga"
                      :weight 'normal
                      :height 150)

  ;; full screen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(undecorated . t))

  (defalias 'yes-or-no-p 'y-or-n-p)
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
    (load custom-file)))

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
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
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
  (winner-mode)
  :bind (("C-x C-u" . winner-undo)
         ("C-x u" . winner-undo)))

(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (isearch-allow-scroll 'unlimited)
  :bind (:map isearch-mode-map
              ("C-c C-o" . isearch-occur)))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 3000)
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
  :custom 
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :bind (("C-t" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-g" . consult-git-grep)
         ("C-x C-i" . consult-imenu-multi)
         ("C-c C-i" . consult-imenu)
         ("M-j M-f" . consult-flymake)
         ("C-x j" . consult-bookmark)))

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
  :bind (("C-x f" . project-find-file)))

(use-package hl-todo
  :custom (hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

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
  :bind (("C-x g" . magit-status)
         ("C-c C-g" . magit-status)))

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

(use-package breadcrumb
  :straight (breadcrumb :type git :host github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode))

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
              ;; ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)
              ("RET" . nil)
              ("M-r" . nil)
              ("C-j" . nil))
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
  ;; use lsp
  ;; (add-hook 'cider-mode-hook
  ;;           (lambda ()
  ;;             (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)))
  (defun +eglot-completion-at-point ()
    (when (boundp 'eglot-completion-at-point)
      (funcall 'eglot-completion-at-point)))
  
  (defalias 'cape-cider-eglot
    (cape-super-capf #'cider-complete-at-point
                     #'+eglot-completion-at-point))

  (defun mpenet/cider-capf ()
    (add-to-list 'completion-at-point-functions
                 #'cape-cider-eglot))
 
  :hook ((cider-mode . mpenet/cider-capf)
         (cider-repl-mode . mpenet/cider-capf)
         (cider-repl-mode . company-mode))
  :bind (:map cider-mode-map
              ("C-c C-d" . cider-debug-defun-at-point)
              ("C-c d" . cider-debug-defun-at-point)))

;;; eglot

(use-package eglot
  :straight (eglot :source gnu-elpa-mirror)
  :ensure t
  :commands (eglot eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:inherit 'highlight :background "#434C5E"))))
  :custom
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-extend-to-xref t)
  (eglot-events-buffer 0)
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (before-save . eglot-format-buffer))

  :bind (:map eglot-mode-map
              ("M-l M-l" . eglot-code-actions))
  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (diminish 'eldoc-mode)
  (advice-add 'eglot--format-markup
              :around (lambda (orig &rest args) (let ((inhibit-read-only t)) (apply orig args)))))

(use-package jarchive
  :straight (jarchive :type git
                      :host nil
                      :repo "https://git.sr.ht/~dannyfreeman/jarchive")
  :hook ((clojure-mode . jarchive-setup)
         (clojurec-mode . jarchive-setup)))

(use-package company
  :diminish
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("TAB" . company-complete-selection))
  
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 3)
  (company-require-match nil)
  (company-idle-delay 0.1)

  :hook (prog-mode . company-mode))

(use-package company-quickhelp
  :custom (company-quickhelp-use-propertized-text t)
  :config (company-quickhelp-mode))

(use-package flymake
  :custom-face (flymake-end-of-line-diagnostics-face ((t :height 0.8 :box nil :slant italic)))
  :custom (flymake-show-diagnostics-at-end-of-line 'short))

(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :custom
  (js-indent-level tab-width))

(use-package fennel-mode
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
  :custom (markdown-fontify-code-blocks-natively t)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
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

(use-package yasnippet
  :custom
  (yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt))
  (yas-indent-line nil)
  :config
  (yas-global-mode t)
  :diminish yas-minor-mode)

(use-package clojure-snippets)

(use-package doom-modeline
  :custom
  (doom-modeline-checker-simple-format nil) ; show full flymake info
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  :init (doom-modeline-mode 1))

(use-package minions
  :disabled
  :custom (minions-prominent-modes '(flymake-mode))
  :config (minions-mode 1))

(use-package doom-themes
  :config
  (let ((theme 'doom-nord))
    (load-theme theme t)
    (enable-theme theme))
  (set-face-attribute 'compilation-warning nil :slant 'normal))

(use-package padded-modeline
  :disabled   
  :straight
  (padded-modeline :type git
                   :host github
                   :repo "mpenet/padded-modeline"
                   :branch "main")
  :custom (padded-modeline-padding 6)
  :config
  (padded-modeline-mode t))

(use-package hl-line
  :config (global-hl-line-mode))

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
  :diminish
  :hook ((text-mode prog-mode conf-mode) . jinx-mode)
  :bind (("M-j c" . jinx-correct)
         ("M-j l" . jinx-languages)))

(use-package scratch)

(use-package dockerfile-mode)

(use-package eshell
  :after esh-mode
  :bind (:map eshell-mode-map
              ("C-r" . consult-history)))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package sudo-edit)

(use-package org-modern
  :custom (org-startup-indented t)
  :after org-roam
  :hook (org-mode . org-modern-mode))

(use-package org-roam
  :custom
  (org-roam-v2-ack t)
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

(use-package copilot
  :disabled
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind (("M-c" . nil)
         ("M-c M-c" . copilot-complete)         
         ("M-c M-a" . copilot-accept-completion)         
         ("M-c M-p" . copilot-next-completion)         
         ("M-c M-n" . copilot-previous-completion)                                       
         ("M-c M-g" . copilot-clear-overlay)))

