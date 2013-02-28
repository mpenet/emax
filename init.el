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

;; globals
(setq base-dir "~/.emacs.d/"
      default-tab-size 4)
(add-to-list 'load-path base-dir)

(require 'utils)
(defalias 'concat-base (apply-partially 'concat base-dir))

;; my mode/bindings etc
(load-files-in-dir (concat-base "config"))
