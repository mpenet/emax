;; misc globals
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
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      skeleton-pair t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<C-M-tab>")
                (lambda ()
                  (interactive)
                  (indent-rigidly (region-beginning)
                                  (region-end)
                                  (- tab-width))
                  (setq mark-active t deactivate-mark nil)))
(global-set-key (kbd "<C-tab>")
                (lambda ()
                  (interactive)
                  (indent-rigidly (region-beginning)
                                  (region-end)
                                  tab-width)
                  (setq mark-active t deactivate-mark nil)))
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
(global-set-key (kbd "C-x '") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)
(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key "\C-x\C-g" 'rgrep)
(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)
(global-set-key (kbd "C-.") 'find-tag)
(global-set-key (kbd "C-,") 'pop-tag-mark)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
                               try-expand-dabbrev-all-buffers
                               try-expand-dabbrev-from-kill
                               try-complete-file-name-partially
                               try-complete-file-name
                               try-expand-all-abbrevs
                               try-expand-list
                               try-expand-line
                               try-complete-lisp-symbol-partially
                               try-complete-lisp-symbo))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "M-u") 'hippie-expand)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcase system-name
  ("dixie" (set-frame-font "-xos4-terminus-medium-r-normal-*-14-*-*-*-*-*-*-1"))
  (_ (set-frame-font "-xos4-terminus-medium-r-normal-*-12-*-*-*-*-*-*-1")))

(setq ns-use-system-highlight-color nil
      ns-pop-up-frames nil)

(global-font-lock-mode 1)

;; show fun in modeline
(which-function-mode)

;; utf8 only
(setq current-language-environment "UTF-8"
      slime-net-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; tabs are evil
(setq-default indent-tabs-mode nil)

;; TAB => 4*'\b'
(setq-default tab-width 4)
(setq-default c-basic-offset tab-width)
(setq-default sgml-basic-offset tab-width)

;; ui
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

;; dont display cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;; add the current line number to the mode bar
(line-number-mode t)

;; add the current column number to the mode bar
(column-number-mode t)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(display-time-mode)

;; make emacs use the clipboard if running in X
(when window-system
  (setq x-select-enable-clipboard t
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

(unless package-archive-contents
  (package-refresh-contents))

(unless package--initialized
  (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

(use-package paren
  :config
  (show-paren-mode +1))

;; uniquify buffer names: append path if buffer names are identical
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode)
  :bind (("C-x C-u" . winner-undo)
         ("C-x u" . winner-undo)
         ("C-x C-j" . winner-redo)
         ("C-x j" . winner-redo)))

(use-package smex
  :pin "melpa-stable"
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("C-x m" . smex)))

(use-package ido
  :ensure t
  :init (progn
          (ido-mode)
          (ido-everywhere))
  :config
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-auto-merge-work-directories-length -1
        ido-case-fold  t
        ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
        ido-ignore-files '("\\.(pyc|jpg|png|gif)$"))
  :bind ("C-x b" . ido-switch-buffer))

(use-package whitespace
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face trailing lines-tail)
        whitespace-global-modes '(not erc-mode)
        whitespace-line-column 80))

(use-package find-file-in-project
  :ensure t
  :config
  (progn
    (setq ffip-patterns '("*")
          ffip-ignore-patterns (list "*/\.*" "*/classes/*" "*/target/*"
                                     "*\\.jar$" "*\\.gif$" "*\\.jpg$" "*\\.png$"
                                     "*\\.log$" "*\\.css$")
          ffip-find-options (mapconcat (lambda (p) (format "-not -iwholename \"%s\"" p))
                                       ffip-ignore-patterns " ")))
  :bind (("C-x f" . find-file-in-project)))

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
  :ensure t
  :init
  (setq company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-require-match nil
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
	                        company-preview-frontend
	                        company-echo-metadata-frontend)
        pos-tip-foreground-color "black"
        pos-tip-background-color "khaki1")
  :config
  (custom-set-faces
   `(company-preview
     ((t (:background "#3F3F3F" :foreground "darkgray" :underline t))))
   `(company-preview-common
     ((t (:background "#3F3F3F" :foreground "darkgray" :underline t)))))
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
  :config
  (loop for mode-hook
        in '(emacs-lisp-mode-hook
             lisp-interaction-mode-hook
             clojure-mode-hook
             cider-mode-hook
             cider-repl-mode-hook
             erlang-mode-hook)
        do (add-hook mode-hook #'paredit-mode))
  (defun my-paredit-delete ()
    "If a region is active check if it is balanced and delete it otherwise
        fallback to regular paredit behavior"
    (interactive)
    (if mark-active
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete)))

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
  ;; overide org-mode tab behavior
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

(use-package restclient
  :ensure t
  :mode ("\\.http$". restclient-mode))

(use-package gist
  :pin "melpa-stable"
  :ensure t)

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
