;; source control
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key "\C-c\C-g" 'magit-status)

;; yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
      yas-indent-line nil)

(yas-global-mode 1)

;; autocomplete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat-base "ac-dict"))
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode nrepl-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (and (not (minibufferp (current-buffer)))
                                (not (eq 'erc-mode major-mode)))
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)


;; js-mode
(eval-after-load 'js-mode
  '(progn
     (setq js-indent-level default-tab-size)
     (add-hook 'js-mode-hook 'yas-minor-mode)))


;; css
(eval-after-load 'css-mode
  '(progn (add-hook 'css-mode-hook 'rainbow-mode)
          (setq css-indent-offset default-tab-size)))


;; less
(eval-after-load 'less-css-mode
  '(progn (setq-default less-css-output-directory "../css/")
          (setq less-css-compile-at-save t
                less-css-lessc-options '("--yui-compress"))))


;; web utilities
(eval-after-load 'zencoding-mode '(setq zencoding-preview-default nil))
(add-hook 'sgml-mode-hook 'zencoding-mode)


;; org
(eval-after-load 'org-mode
    '(progn
       ;; overide org-mode tab behavior
       (defun yas-org-very-safe-expand ()
         (let ((yas-fallback-behavior 'return-nil))
           (yas-expand)))
       (add-hook 'org-mode-hook
                 (lambda ()
                   (make-variable-buffer-local 'yas-trigger-key)
                   (setq yas-trigger-key [tab])
                   (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
                   (define-key yas-keymap [tab] 'yas-next-field)))))
(eval-after-load 'htmlize '(setq org-export-htmlize-output-type 'css))


;; github markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; clojure

(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

;; (defclojureface clojure-parens       "DimGrey"   "Clojure parens")
;; (defclojureface clojure-braces       "yellow"   "Clojure braces")
(defclojureface clojure-brackets     "orange" "Clojure brackets")
(defclojureface clojure-keyword       "#bfebbf"    "Clojure keywords")
;; (defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
;; (defclojureface clojure-special      "#b8bb00"   "Clojure special")
;; (defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (dolist (x '((("#?['`]*(\\|)"       . 'clojure-parens))
               (("#?\\^?{\\|}"        . 'clojure-brackets))
               (("\\[\\|\\]"          . 'clojure-braces))
               ((":\\w+#?"            . 'clojure-keyword))
               (("#?\""               0 'clojure-double-quote prepend))
               (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
               (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call))
               ))
    (font-lock-add-keywords nil x)))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)

(eval-after-load 'clojure-mode
  '(progn
     ;; (global-set-key (kbd "<f9>") 'slime-connect)
     ;; (global-set-key (kbd "<f10>") 'elein-swank)
     ;; (global-set-key (kbd "<f11>") 'elein-kill-swank)
     ))


;; paredit
(loop for mode-hook
      in '(emacs-lisp-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           nrepl-mode-hook)
      do (add-hook mode-hook (lambda () (paredit-mode +1))))

(eval-after-load 'paredit
  '(progn
     (defun my-paredit-delete ()
       "If a region is active check if it is balanced and delete it otherwise
        fallback to regular paredit behavior"
       (interactive)
       (if mark-active
           (paredit-delete-region (region-beginning) (region-end))
         (paredit-backward-delete)))
     (define-key paredit-mode-map (kbd "<delete>") 'my-paredit-delete)
     (define-key paredit-mode-map (kbd "DEL") 'my-paredit-delete)))


(autoload 'ac-nrepl "ac-nrepl-setup")
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;; (setq nrepl-popup-stacktraces nil)
(add-hook 'nrepl-mode-hook 'subword-mode)

;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)


;; haskell
(eval-after-load 'haskell-mode
  '(progn
          ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
          ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
	  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
	  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))


;; buffers, project, files navigation (should refactor this into a mode)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t
      ido-auto-merge-work-directories-length -1
      ido-case-fold  t
      ido-ignore-buffers
      '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
      ido-ignore-files '("\\.(pyc|jpg|png|gif)$"))
(global-set-key "\C-x\C-b" 'ido-switch-buffer) ;; disable annoying buffer menu

;; uniquify buffer names: append path if buffer names are identical
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;; find-file-in-project
(eval-after-load 'find-file-in-project
  '(progn
     (setq ffip-patterns '("*")
           ffip-ignore-patterns (list "*/\.*" "*/classes/*" "*/target/*"
                                      "*\\.jar$" "*\\.gif$" "*\\.jpg$" "*\\.png$"
                                      "*\\.log$" "*\\.css$")
           ffip-find-options (mapconcat (lambda (p) (format "-not -iwholename \"%s\"" p))
                                        ffip-ignore-patterns " "))))
(global-set-key (kbd "C-x f") 'find-file-in-project)


;; smex
(smex-initialize)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key (kbd "C-x m") 'smex)


;; winner-mode
(winner-mode 1)
(setq winner-dont-bind-my-keys t)
(global-set-key "\C-x\C-u" 'winner-undo)
(global-set-key (kbd "C-x u") 'winner-undo)
(global-set-key "\C-x\C-j" 'winner-redo)
(global-set-key (kbd "C-x j") 'winner-redo)


;; hippie expand + dabbrev-expand
(global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
                               try-expand-dabbrev-all-buffers
                               try-expand-dabbrev-from-kill
                               try-complete-lisp-symbol-partially
                               try-complete-lisp-symbol
                               try-complete-file-name-partially
                               try-complete-file-name))

;; eshell
(setq eshell-directory-name (concat-base "extras/eshell"))
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key "\C-x\C-t" 'eshell)


;; disable arrows & co
(guru-global-mode)


;; poor mans paredit
(show-paren-mode t)
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)


;; apply the PATH environment variable to Emacs and set the exec-path
(exec-path-from-shell-initialize)
