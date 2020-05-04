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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK & FEEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(font . "JetBrainsMono 13"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Gathered from https://www.jetbrains.com/lp/mono/#ligatures
;; The cheatsheat shows "\/" and "\" which I couldn't get working
(let ((alist '(;;  -> -- --> ->> -< -<< --- -~ -|
               (?- . ".\\(?:--\\|[->]>?\\|<<?\\|[~|]\\)")

               ;; // /* /// /= /== />
               ;; /** is not supported - see https://github.com/JetBrains/JetBrainsMono/issues/202
               ;; /* cannot be conditioned on patterns followed by a whitespace,
               ;; because that would require support for lookaheads in regex.
               ;; We cannot just match on /*\s, because the whitespace would be considered
               ;; as part of the match, but the font only specifies the ligature for /* with
               ;; no trailing characters
               ;;
               (?/ . ".\\(?://?\\|==?\\|\\*\\*?\\|[>]\\)")

               ;; */ *** *>
               ;; Prevent grouping of **/ as *(*/) by actively looking for **/
               ;; which consumes the triple but the font does not define a substitution,
               ;; so it's rendered normally
               (?* . ".\\(?:\\*/\\|\\*\\*\\|[>/]\\)")

               ;; <!-- <<- <- <=> <= <| <|| <||| <|> <: <> <-< <<< <=< <<= <== <==>
               ;; <~> << <-| <=| <~~ <~ <$> <$ <+> <+ <*> <* </ </> <->
               (?< . ".\\(?:==>\\|!--\\|~~\\|-[|<]\\||>\\||\\{1,3\\}\\|<[=<-]?\\|=[><|=]?\\|[*+$~/-]>?\\|[:>]\\)")

               ;; := ::= :?> :? :: ::: :< :>
               (?: . ".\\(?:\\?>\\|:?=\\|::?\\|[>?<]\\)")

               ;; == =:= === => =!= =/= ==> =>>
               (?= . ".\\(?:[=>]?>\\|[:=!/]?=\\)")

               ;;  != !== !!
               (?! . ".\\(?:==?\\|!\\)")

               ;; >= >> >] >: >- >>> >>= >>- >=>
               (?> . ".\\(?:=>\\|>[=>-]\\|[]=:>-]\\)")

               ;; && &&&
               (?& . ".&&?")

               ;; || |> ||> |||> |] |} |-> |=> |- ||- |= ||=
               (?| . ".\\(?:||?>\\||[=-]\\|[=-]>\\|[]>}|=-]\\)")

               ;; ... .. .? .= .- ..<
               (?. . ".\\(?:\\.[.<]?\\|[.?=-]\\)")

               ;; ++ +++ +>
               (?+ . ".\\(?:\\+\\+?\\|>\\)")

               ;; [| [< [||]
               (?\[ . ".\\(?:|\\(?:|]\\)?\\|<\\)")

               ;; {|
               (?{ . ".|")

               ;; ?: ?. ?? ?=
               (?? . ".[:.?=]")

               ;; ## ### #### #{ #[ #( #? #_ #_( #: #! #=
               (?# . ".\\(?:#\\{1,3\\}\\|_(?\\|[{[(?:=!]\\)")

               ;; ;;
               (?\; . ".;")

               ;; __ _|_
               (?_ . ".|?_")

               ;; ~~ ~~> ~> ~= ~- ~@
               (?~ . ".\\(?:~>\\|[>@=~-]\\)")

               ;; $>
               (?$ . ".>")

               ;; ^=
               (?^ . ".=")

               ;; ]#
               (?\] . ".#")
               )))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))



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

(setq-default cursor-in-non-selected-windows nil)

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

;;; Packages
;;; via straight el

(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      straight-built-in-pseudo-packages '(which-function-mode
                                          isearch
                                          dired
                                          js-mode
                                          erc-log
                                          uniquify
                                          inferior-lisp))

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
        isearch-allow-scroll 'unlimited))

(use-package ivy
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
   ivy-count-format "(%d/%d) "
   ivy-magic-tilde nil
   ivy-dynamic-exhibit-delay-ms 150
   ivy-re-builders-alist '((swiper . regexp-quote)
                           (counsel-M-x . ivy--regex-fuzzy)
                           (counsel-git . ivy--regex-fuzzy)
                           (t . ivy--regex-plus))))

(use-package swiper
  :bind (("\C-t" . swiper-isearch)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x m" . counsel-M-x)
         ("C-x C-g" . counsel-rg)
         ("C-x f" . counsel-git)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-extra-directories nil))

(use-package smex)

(use-package diminish
  :demand t)

(use-package whitespace
  :diminish
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face trailing lines-tail)
        whitespace-global-modes '(not erc-mode)
        whitespace-line-column 80))

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
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package magit
  ;; :pin "melpa-stable"
  :bind (("C-x g" . magit-status)
         ("C-c C-g" . magit-status)))

(use-package yasnippet
  :diminish
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode t)
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
        yas-indent-line nil)
  :diminish yas-minor-mode)

(use-package company-quickhelp)

(use-package company
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
  (define-key paredit-mode-map (kbd "C-j") nil)
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-backward-kill-word)
              ("C-h" . my-paredit-delete)
              ("<delete>" . my-paredit-delete)
              ("DEL" . my-paredit-delete)))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package company-go
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-mode
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

(use-package go-autocomplete)

(use-package go-gopath)

(use-package go-snippets)

(use-package go-eldoc
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package js-mode
  :defer t
  :mode ("\\.json$" . js-mode)
  :config
  (setq js-indent-level tab-width)
  (add-hook 'js-mode-hook 'yas-minor-mode))

(use-package fennel-mode
  :hook (fennel-mode . paredit-mode)
  :bind (:map fennel-mode-map ("C-c C-c" . lisp-eval-defun)))

(use-package inferior-lisp
  :hook (inferior-lisp-mode . paredit-mode))

(use-package lua-mode)

(use-package nginx-mode)

(use-package doom-modeline
  :config
  (setq doom-modeline-irc-buffers t
        doom-modeline-irc t
        doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy
  :hook (after-init . all-the-icons-ivy-setup))

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package css-mode
  :config
  (progn (add-hook 'css-mode-hook 'rainbow-mode)
         (setq css-indent-offset tab-width)))

(use-package zencoding-mode
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
  :config (setq org-export-htmlize-output-type 'css))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

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
  (load-theme 'doom-wilmersdorf t))

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
              ("C-;" . comment-or-uncomment-region)))

(use-package docker)
(use-package dockerfile-mode)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package clojure-snippets)

(use-package erc
  :commands (erc erc-tls)
  :requires (erc-services)
  :preface
  (defun setup-erc-env ()
    (setq-default erc-enable-logging 'erc-log-all-but-server-buffers)
    (setq erc-modules '(netsplit fill track completion ring button autojoin
                                 services match stamp track page scrolltobottom
                                 hl-nicks irccontrols spelling)
          erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
          erc-autojoin-mode t
          erc-timestamp-format "%H:%M "
          erc-interpret-mirc-color t
          erc-input-line-position -2
          erc-prompt ">"
          erc-keywords '("\\bmpenet\\b")
          erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-current-nick-highlight-type 'nick
          erc-prompt-for-nickserv-password nil
          erc-autojoin-channels-alist exo-irc-channels
          erc-server-auto-reconnect t
          erc-track-exclude exo-irc-track-exclude))

  (defun kikoo-line (nick)
    (let ((front (+ (random 13) 1))
          (back (+ (random 13) 1)))
      (while (= back front)
        (setq back (+ (random 13) 1)))
      (erc-send-message (apply 'concat
                               (append (list (format "\002\003%s,%sKIKOO"
                                                     front
                                                     back))
                                       (make-list (+ (random 11) 2) "O")
                                       (list " " nick " ")
                                       (make-list (+ (random 11) 2) "!")
                                       (list " :o")
                                       (make-list (+ (random 11) 2) ")")))
                        t)))

  (defun kikoo ()
    (interactive)
    (let ((nick (completing-read "Enter nick: " (erc-get-channel-nickname-list))))
      (dotimes (i 3)
        (kikoo-line nick))))

  (defun irc/exo ()
    "Connect to ERC, or switch to last active buffer."
    (interactive)
    (load "~/.emacs.d/.secrets.el")
    (setup-erc-env)
    (erc-log-mode)
    (erc-tls :server exo-irc-server-host
             :port exo-irc-server-port
             :nick exo-irc-nick
             :full-name exo-irc-full-name
             :password exo-irc-password))
  :config
  (erc-services-mode 1)
  (erc-track-mode t))

(use-package erc-log
  :init
  (setq erc-log-channels-directory "~/.erc/logs/"
        ;; erc-save-buffer-on-part nil
        ;; erc-save-queries-on-quit nil
        ;; erc-log-write-after-send t
        ;; erc-log-write-after-insert t
        )
  (if (not (file-exists-p erc-log-channels-directory))
      (mkdir erc-log-channels-directory t)))

(use-package erc-hl-nicks
  :after irc/exo)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
