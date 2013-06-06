;; possibly not running on emacs 24
(when (not (require 'package nil t))
  (require 'package "package-23.el"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(melpa
                      ac-nrepl
                      auto-complete
                      clojure-mode
                      clojure-snippets
                      clojure-test-mode
                      dash
                      ;; elixir-mode
                      erc-hl-nicks
                      ;; erlang
                      exec-path-from-shell
                      find-file-in-project
                      guru-mode
                      haskell-mode
                      htmlize
                      less-css-mode
                      lein
                      magit
                      markdown-mode
                      nginx-mode
                      nrepl
                      ;; nrepl-ritz
                      paredit
                      rainbow-mode
                      ;; roy-mode
                      rust-mode
                      smex
                      yaml-mode
                      yasnippet
                      zenburn-theme
                      zencoding-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path user-emacs-directory)

(require 'utils)
(defalias 'concat-base (apply-partially 'concat user-emacs-directory))

;; load pwds and other sensitive stuff
(load "~/.emacs-secrets" t)

;; my mode/bindings etc
(load-files-in-dir (concat-base "config"))
