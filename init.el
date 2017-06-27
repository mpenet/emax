;; possibly not running on emacs 24
(when (not (require 'package nil t))
  (require 'package "package-23.el"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cargo
                      cider
                      clojure-mode
                      clojure-snippets
                      company
                      company-quickhelp
                      company-ghc
                      dash
                      flycheck
                      flycheck-dialyzer
                      ;; flycheck-rebar3
                      flycheck-pos-tip
                      docker
                      docker-tramp
                      dropdown-list
                      erc-hl-nicks
                      erlang
                      exec-path-from-shell
                      find-file-in-project
                      flymake-rust
                      ;; flycheck-ocaml
                      gist
                      go-mode
                      go-autocomplete
                      go-eldoc
                      go-gopath
                      go-snippets
                      company-go

                      guru-mode
                      htmlize
                      kotlin-mode
                      flycheck-kotlin
                      less-css-mode
                      magit
                      markdown-mode
                      merlin
                      nginx-mode
                      ocp-indent
                      ox-reveal
                      paredit
                      racer
                      rainbow-mode
                      restclient
                      rust-mode
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

(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
