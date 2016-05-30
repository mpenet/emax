;; possibly not running on emacs 24
(when (not (require 'package nil t))
  (require 'package "package-23.el"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      clojure-mode
                      clojure-snippets
                      company
                      company-quickhelp
                      dash
                      flycheck
                      ;; flycheck-dialyzer
                      flycheck-pos-tip
                      dropdown-list
                      erc-hl-nicks
                      erlang
                      exec-path-from-shell
                      find-file-in-project
                      flymake-haskell-multi
                      flymake-rust
                      flycheck-hdevtools
                      ;; flycheck-ocaml
                      guru-mode
                      ghc
                      ghci-completion
                      haskell-mode
                      htmlize
                      hy-mode
                      less-css-mode
                      magit
                      markdown-mode
                      merlin
                      nginx-mode
                      ocp-indent
                      paredit
                      pixie-mode
                      rainbow-mode
                      restclient
                      rust-mode
                      shm
                      slime
                      slime-company
                      smex
                      tuareg
                      yaml-mode
                      yasnippet
                      zenburn-theme
                      zencoding-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'dash)
(defun add-subdirs-to-list (list-var path)
  (let ((modules-dir path))
    (add-to-list list-var modules-dir)
    (dolist (f (directory-files modules-dir))
      (let ((name (concat modules-dir "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list list-var name))))))

(defun load-files-in-dir (dir)
  (let ((file-in-dir-p '(lambda (x) (file-regular-p (format "%s/%s" dir x)))))
    (dolist (f (-filter file-in-dir-p (directory-files dir)))
      (load-file (format "%s/%s" dir f)))))

(defalias 'concat-base (apply-partially 'concat user-emacs-directory))

;; load pwds and other sensitive stuff
(load "~/.emacs-secrets" t)

;; my mode/bindings etc
(load-files-in-dir (concat-base "elisp"))
