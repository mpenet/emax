;; C-u 0 M-x byte-recompile-directory

;; add base plugin dir + subdirs to load-path
(let ((base "~/.emacs.d/elisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; source control
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key "\C-c\C-g" 'magit-status)

;; yasnippet
(require 'yasnippet)
(require 'dropdown-list)
(yas/initialize)
;(set-face-background 'yas/field-highlight-face "gray")
(yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets/")
(setq yas/prompt-functions '(yas/dropdown-prompt yas/x-prompt))
(setq yas/indent-line nil)


;; autocomplete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)
;; (setq ac-auto-start nil)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(set-default 'ac-sources
             '(ac-source-yasnippet
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (and (not (minibufferp (current-buffer)))
                                (not (eq 'erc-mode major-mode)))
                         (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

;; js-mode
(setq js-indent-level 4)
(add-hook 'js-mode-hook 'yas/minor-mode)

;; web utilities
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(setq zencoding-preview-default nil)

;; clojure
(require 'clojure-mode)

;; clojurescript compile after-save
(defun compile-cljs-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (let* ((file-name (buffer-file-name))
                      (file-name-dir (file-name-directory file-name)))
                 (if (string-match "\.cljs$" file-name)
                     (async-shell-command
                      (concat "cljsc " file-name-dir " '{:optimizations :advanced}' > "
                              file-name-dir "compiled.js") nil nil))))))
(add-hook 'clojure-mode-hook 'compile-cljs-on-after-save-hook)
(setq auto-mode-alist (cons '("\\.cljs$" . clojure-mode) auto-mode-alist))

;; paredit
(require 'paredit)
(loop for mode-hook
      in '(emacs-lisp-mode-hook
           scheme-mode-hook
           clojure-mode-hook
           js-mode-hook)
      do (add-hook mode-hook 'enable-paredit-mode))

;; dont insert space before a paren on js files
(add-hook 'js-mode-hook
          '(lambda ()
             (add-to-list (make-local-variable
                           'paredit-space-for-delimiter-predicates)
                          (lambda (_ _) nil))))

(defun my-paredit-delete ()
  "If a region is active check if it is balanced and delete it otherwise
   fallback to regular paredit behavior"
  (interactive)
  (if mark-active
      (paredit-delete-region (region-beginning) (region-end))
    (paredit-backward-delete)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<delete>") 'my-paredit-delete)
     (define-key paredit-mode-map (kbd "DEL") 'my-paredit-delete)))

;; leiningen
(require 'elein)
(global-set-key (kbd "<f9>") 'slime-connect)
(global-set-key (kbd "<f10>") 'elein-swank)
(global-set-key (kbd "<f11>") 'elein-kill-swank)

;; slime + swank-clojure
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl slime-fancy))
          (setq slime-protocol-version 'ignore)))
(require 'slime)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'slime-connected-hook 'slime-redirect-inferior-output)

;; haskell
(require 'haskell-mode)
(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; tramp
(setq tramp-default-method "ssh")

;; css
(setq css-indent-offset 4)

;; less
(defun compile-less-on-after-save-hook ()
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (let ((file-name (buffer-file-name)))
                 (if (string-match "\.less$" file-name)
                     (async-shell-command
                      (concat "lessc " file-name " "
                              (file-name-directory file-name) "../css/"
                              (file-name-sans-extension (file-name-nondirectory
                                                         file-name))
                              ".css") nil nil))))))
(add-hook 'css-mode-hook 'compile-less-on-after-save-hook)
(setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))

;; font
(set-frame-font "-xos4-terminus-medium-r-normal-*-12-*-*-*-*-*-*-1")
(setq ns-use-system-highlight-color nil)
(setq ns-pop-up-frames nil)
(global-font-lock-mode 1)

;; theme
(require 'color-theme)
(require 'color-theme-zenburn)
(setq color-theme-is-global t)
;;(color-theme-initialize)
(color-theme-zenburn)
(set-face-background 'region "dark slate blue")
;(color-theme-tangotango)

;; org-mode
(require 'org-install)
(require 'htmlize)
;overwrite org-mode tab behavior
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(setq org-export-htmlize-output-type 'css)
;;       org-confirm-babel-evaluate nil
;;       org-src-fontify-natively t
;;       org-src-tab-acts-natively t
;;       )


;; buffers & files navigation
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

; uniquify buffer names: append path if buffer names are identical
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; quick project file navigation
(require 'filecache)

(setq file-cache-ignore-patterns (list "/[.]git" "/[.]svn" "\\.svn-base$"
                                       "\\.jar$" "\\.gif$" "\\.jpg$" "\\.png$"
                                       "\\.log$"))
(loop for pattern in file-cache-ignore-patterns
      do (add-to-list 'file-cache-filter-regexps pattern))

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

;; filter out version control files
(global-set-key (kbd "\C-x f") 'file-cache-ido-find-file)

(require 'recentf)
(setq recentf-max-saved-items 100)

;; kb shortcuts
(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "<f4>") 'start-or-end-kbd-macro)
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)
(global-set-key "\C-x\C-k" 'kill-buffer)
(global-set-key "\C-x\C-o" 'other-window)

(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key "\C-x\C-g" 'rgrep)

(global-set-key (kbd "C-x r") 'query-replace)
(global-set-key "\C-x\C-r" 'query-replace)

(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key "\C-x\C-l" 'goto-line)

(global-set-key (kbd "C-x a") 'align-regexp)
(global-set-key "\C-x\C-a" 'align-regexp)

(global-set-key (kbd "C-c p") 'check-parens)
(global-set-key "\C-c\C-p" 'check-parens)

(global-set-key "\C-x n" 'rename-buffer)
(global-set-key "\C-x\C-n" 'rename-buffer)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(defun open-term ()
  (interactive)
  (ansi-term "bash"))

(global-set-key (kbd "C-x t") 'open-term)
(global-set-key "\C-x\C-t" 'open-term)


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

;; ERC
(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.quakenet.net:6667") ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (erc :server "irc.quakenet.org" :port 6667 :nick "zcam" :full-name "zcam")))

(erc-track-mode t)

(setq erc-modules '(netsplit fill track completion ring button autojoin
                             services match stamp track page scrolltobottom)
      erc-autojoin-mode t
      erc-timestamp-format "%H:%M "
      erc-interpret-mirc-color t
      erc-input-line-position -2
      erc-prompt ">>"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-prompt-for-nickserv-password nil)

(global-set-key (kbd "C-c e") 'djcb-erc-start-or-switch)


;; look 'Ma no arrows
(defvar no-easy-keys-minor-mode-map (make-keymap)
  "no-easy-keys-minor-mode keymap.")
(let ((f (lambda (m)
           `(lambda () (interactive)
              (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
               ("<down>" . "C-n")
               ("<C-left>" . "M-f") ("<C-right>" . "M-b") ("<C-up>" . "M-{")
               ("<C-down>" . "M-}")
               ("<M-left>" . "M-f") ("<M-right>" . "M-b") ("<M-up>" . "M-{")
               ("<M-down>" . "M-}")
               ("<delete>" . "C-d") ("<C-delete>" . "M-d")
               ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
               ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
               ("<home>" . "C-a") ("<C-home>" . "M->")
               ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key no-easy-keys-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))
(define-minor-mode no-easy-keys-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
  and backspace."  t " no-easy-keys"
  'no-easy-keys-minor-mode-map :global t)
(no-easy-keys-minor-mode 1)

;; macros
(defun start-or-end-kbd-macro ()
  "Starts recording a keyboard macro, or if already recording,
   stops recording it."
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

;; Turn off unncessary ui stuff
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; utf8 only
(setq current-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)


;; highlight the region between the mark and point
(transient-mark-mode t)

;; dont display cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;; poor mans paredit
(show-paren-mode t)
(set-face-foreground 'show-paren-match-face "red")
;(set-face-background 'show-paren-match-face "black")

(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;; add the current line number to the mode bar
(line-number-mode t)

;; add the current column number to the mode bar
(column-number-mode t)

;; highlight long lines tails
(setq whitespace-style (quote (lines-tail))
      whitespace-line-column 80)
(global-whitespace-mode 1)

;; no splash screen
(setq inhibit-startup-message t)

;; empty scratch buffer
(setq initial-scratch-message nil)

;; shhhh! just flash minibuffer
(setq visible-bell t)

;; Don't make me type out 'yes', 'y' is good enough.
(fset 'yes-or-no-p 'y-or-n-p)

;; tabs are evil
(setq-default indent-tabs-mode nil)

;; TAB => 4*'\b'
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default sgml-basic-offset 4)

;; hassle free indent
(defun my-unindent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) (- tab-width))
  (setq mark-active t deactivate-mark nil))
(global-set-key (kbd "<C-M-tab>") 'my-unindent)

(defun my-indent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) tab-width)
  (setq mark-active t deactivate-mark nil))
(global-set-key (kbd "<C-tab>") 'my-indent)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c b") 'cleanup-buffer)
(global-set-key (kbd "C-c C-b") 'cleanup-buffer)

;; show me everything
(setq truncate-partial-width-windows nil)

;; case insensitive searches
(set-default 'case-fold-search t)

;; typed text replaces the selection if the selection is active
(delete-selection-mode t)

;; make emacs use the clipboard if running in X
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)

;disable auto save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; global save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; automate make-directory if necessary
(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))

;; don't save emacs session
(setq save-place nil)

;; apply the PATH environment variable to Emacs and set the exec-path
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; simple project management

(setq project-history-file "~/.emacs.d/project.hist")

(defun project-path-prompt (path)
  (interactive (list
                (ido-read-directory-name
                 "Project root: ")))
  (set-project path))
(global-set-key (kbd "<f12>") 'project-path-prompt)

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

(load-project-from-history)