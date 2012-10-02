;; source control
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key "\C-c\C-g" 'magit-status)


;; yasnippet
(require 'dropdown-list)
(setq yas-snippet-dirs (concat-base "extras/yasnippet/snippets")
      yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
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
                html-mode nxml-mode sh-mode smarty-mode clojure-mode slime-repl-mode
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
          ()
          (setq less-css-compile-at-save t
                less-css-lessc-options "-x")))


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
     (global-set-key (kbd "<f9>") 'slime-connect)
     (global-set-key (kbd "<f10>") 'elein-swank)
     (global-set-key (kbd "<f11>") 'elein-kill-swank)))


;; paredit
(loop for mode-hook
      in '(emacs-lisp-mode-hook
           scheme-mode-hook
           clojure-mode-hook)
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


; slime
(autoload 'slime "slime" nil t)
(autoload 'slime-mode "slime" nil t)
(autoload 'slime-connect "slime" nil t)
(autoload 'slime-set-inferior-process "slime" nil t)
(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl slime-fancy))
     (setq slime-protocol-version 'ignore)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-connected-hook 'slime-redirect-inferior-output)))


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

(autoload 'file-cache-ido-find-file "filecache")
(autoload 'project-path-prompt "filecache")
(eval-after-load 'filecache
  '(progn
     (defun file-cache-ido-find-file (file)
       "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
       (interactive (list (file-cache-ido-read "File: "
                                               (mapcar
                                                (lambda (x)
                                                  (car x))
                                                file-cache-alist))))
       (let* ((record (assoc file file-cache-alist)))
         (find-file
          (expand-file-name
           file
           (if (= (length record) 2)
               (car (cdr record))
             (file-cache-ido-read
              (format "Find %s in dir: " file) (cdr record)))))))

     (defun file-cache-ido-read (prompt choices)
       (let ((ido-make-buffer-list-hook
              (lambda ()
                (setq ido-temp-list choices))))
         (ido-read-buffer prompt)))

     (setq file-cache-ignore-patterns (list "/[.]git" "/[.]svn" "\\.svn-base$"
                                            "\\.jar$" "\\.gif$" "\\.jpg$" "\\.png$"
                                            "\\.log$" "\\.css$"))
     (loop for pattern in file-cache-ignore-patterns
           do (add-to-list 'file-cache-filter-regexps pattern))


     ;; simple project management
     (setq project-history-file (concat-base "project.hist"))

     (defun project-path-prompt (path)
       (interactive (list
                     (ido-read-directory-name
                      "Project root: ")))
       (set-project path))

     (defun set-project (path)
       (save-project-history path)
       (file-cache-clear-cache)
       (file-cache-add-directory-using-find path))

     (defun load-project-from-history ()
       (when (file-exists-p project-history-file)
         (with-temp-buffer
           (insert-file-contents project-history-file)
           (set-project (buffer-string)))))

     (defun save-project-history (project-path)
       (when (file-exists-p project-history-file)
         (delete-file project-history-file))
       (append-to-file project-path nil project-history-file))

     (load-project-from-history)))

(global-set-key (kbd "\C-x f") 'file-cache-ido-find-file)
(global-set-key (kbd "<f12>") 'project-path-prompt)


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
