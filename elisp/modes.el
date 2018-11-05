;; source control
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key "\C-c\C-g" 'magit-status)

(require 'cl)

;; yasnippet
;; (require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt yas-x-prompt)
      yas-indent-line nil)
(eval-after-load 'yasnippet
  '(add-to-list 'yas-snippet-dirs (concat-base "snippets") t))

(yas-global-mode 1)

;; company
(global-company-mode)
(eval-after-load 'company
  (progn
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map [tab] 'company-complete-selection)

    (setq company-tooltip-align-annotations t)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match nil)

    (setq company-frontends
	  '(company-pseudo-tooltip-unless-just-one-frontend
	    company-preview-frontend
	    company-echo-metadata-frontend))
    (company-quickhelp-mode 1)

    ;; meh
    (setq pos-tip-foreground-color "black")
    (setq pos-tip-background-color "khaki1")

    (custom-set-faces
     `(company-preview
       ((t (:background "#3F3F3F" :foreground "darkgray" :underline t))))
     `(company-preview-common
       ((t (:background "#3F3F3F" :foreground "darkgray" :underline t)))))))

(eval-after-load 'go-mode
  '(progn
     (require 'go-autocomplete)
     (require 'go-gopath)

     (add-hook 'go-mode-hook (lambda ()
                               (flycheck-mode)
                               ;; (setq company-tooltip-limit 20)                      ; bigger popup window
                               ;; (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
                               ;; (setq company-echo-delay 0)                          ; remove annoying blinking
                               ;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
                               (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
                               (set (make-local-variable 'company-backends)
                                    '(company-go))))
     (add-hook 'before-save-hook 'gofmt-before-save)))

;; js-mode
(eval-after-load 'js-mode
  '(progn
     (setq js-indent-level tab-width)
     (add-hook 'js-mode-hook 'yas-minor-mode)))


;; css
(eval-after-load 'css-mode
  '(progn (add-hook 'css-mode-hook 'rainbow-mode)
          (setq css-indent-offset tab-width)))


;; less
(eval-after-load 'less-css-mode
  '(progn (setq-default less-css-output-directory "../css/")
          (setq less-css-compile-at-save t
                less-css-lessc-options '("--compress"))))


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
(setq markdown-command "pandoc")

;; rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))


;; paredit
(loop for mode-hook
      in '(emacs-lisp-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           cider-mode-hook
           cider-repl-mode-hook
           erlang-mode-hook
           hy-mode-hook)
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
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "C-h") 'my-paredit-delete)
     (define-key paredit-mode-map (kbd "<delete>") 'my-paredit-delete)
     (define-key paredit-mode-map (kbd "DEL") 'my-paredit-delete)))

(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)
(eval-after-load 'cider
  '(progn
     ;; (setq nrepl-popup-stacktraces nil)
     (setq cider-popup-stacktraces-in-repl t)
     (add-to-list 'exec-path "/usr/local/bin")))


(eval-after-load 'flycheck
  '(progn
    (flycheck-pos-tip-mode)))

(eval-after-load 'erlang
  '(progn
     (require 'flycheck-dialyzer)
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
                   '(mpenet/erlang-paredit-space-for-delimiter-p)))))

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
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-i") 'hippie-expand)
(global-set-key (kbd "M-u") 'hippie-expand)

;; eshell
(setq eshell-directory-name (concat-base "extras/eshell"))

;; poor mans paredit
(show-paren-mode t)
(setq skeleton-pair t)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;; (global-hl-line-mode +1)

;; apply the PATH environment variable to Emacs and set the exec-path
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "LGOBIN")
(setq shell-command-switch "-ic")
(exec-path-from-shell-initialize)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
