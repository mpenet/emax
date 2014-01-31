;; font
(set-frame-font "-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*-1")
(setq ns-use-system-highlight-color nil
      ns-pop-up-frames nil)
(global-font-lock-mode 1)

;; themes
(load-theme 'zenburn t)
(set-face-attribute 'region nil :background "#6f6f6f")
(set-face-foreground 'show-paren-match-face "red")

;; utf8 only
(setq current-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

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
(setq inhibit-startup-message t
      initial-scratch-message nil
      visible-bell t)
(set-fringe-mode 1)

;; highlight the region between the mark and point
(transient-mark-mode t)

;; dont display cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;; add the current line number to the mode bar
(line-number-mode t)

;; add the current column number to the mode bar
(column-number-mode t)

;; highlight long lines tails
(setq whitespace-style '(face trailing lines-tail)
      whitespace-global-modes '(not erc-mode)
      whitespace-line-column 80)
(global-whitespace-mode 1)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

;; make emacs use the clipboard if running in X
(when window-system
  (setq x-select-enable-clipboard t
        interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; misc
(setq tramp-default-method "ssh"
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome"
      ;; show me everything
      truncate-partial-width-windows nil
      ;; disable auto save & backups
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      ;; don't save emacs session
      save-place nil
      vc-follow-symlinks nil)

;; global save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
