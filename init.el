;; possibly not running on emacs 24
(when (not (require 'package nil t))
  (require 'package "package-23.el"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(melpa
                      ac-slime
                      auto-complete
                      clojure-mode
                      clojurescript-mode
                      clojure-test-mode
                      elein
                      guru-mode
                      haskell-mode
                      htmlize
                      less-css-mode
                      magit
                      markdown-mode
                      paredit
                      rainbow-mode
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

;; load stuff that was missing from melpa
(add-subdirs-to-list 'load-path (concat-base "modules"))

;; my mode/bindings etc
(load-files-in-dir (concat-base "config"))
