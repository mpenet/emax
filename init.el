;; C-u 0 M-x byte-recompile-directory

(require 'cl)

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
(yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets/")
(setq yas/prompt-functions '(yas/dropdown-prompt yas/x-prompt))
(setq yas/indent-line nil)

;; js-mode (emacs 23+ default)
(setq js-indent-level 4)
(add-hook 'js-mode-hook 'yas/minor-mode)

;; js2-mode
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-basic-offset 2
;;       js2-use-font-lock-faces t
;;       js2-mode-escape-quotes nil
;;       js2-cleanup-whitespace t
;;       js2-bounce-indent-p t)
;; (global-set-key (kbd "C-c C-n") 'js2-next-error)
;; (add-hook 'js2-mode-hook 'yas/minor-mode)

(defun js2-before-save ()
  "FIXED: dont messup my whitespace again!
  	   Clean up whitespace before saving file.
  	   You can disable this by customizing `js2-cleanup-whitespace'."
  (when js2-cleanup-whitespace
    (let ((col (current-column)))
    (delete-trailing-whitespace)
    (indent-to col))))

;; web utilities
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(setq zencoding-preview-default nil)

;; clojure
(require 'clojure-mode)

;; haskell
(require 'haskell-mode)
(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'slime)
(slime-setup)

(defun clojure-project (path)
  "Setup classpaths for a clojure project and starts a new SLIME session.
  Kills existing SLIME session, if any."
  (interactive (list
                (ido-read-directory-name
                 "Project root: "
                 (locate-dominating-file default-directory "src"))))
  (require 'swank-clojure)
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (add-to-list 'swank-clojure-extra-vm-args
               (format "-Dclojure.compile.path=%s"
                       (expand-file-name "target/classes/" path)))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (expand-file-name "target/dependency/" path)
        swank-clojure-extra-classpaths
        (append (mapcar (lambda (d) (expand-file-name d path))
                        '("src/" "target/classes/" "test/" ))
                (let ((lib (expand-file-name "lib" path)))
                  (if (file-exists-p lib)
                      (directory-files lib t ".jar$"))))
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if #'(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))

(global-set-key (kbd "<f11>") 'clojure-project)

;; tramp
(setq tramp-default-method "ssh")

;; erc
(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.quakenet.net:6667") ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (erc :server "irc.quakenet.org" :port 6667 :nick "scam" :full-name "scam")))

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


;; css
(setq css-indent-offset 4)

;; less with special changes for hw dir structure
(defun compile-less-css ()
  (interactive)
  (if (string-match "\.less$" (buffer-file-name))
      (async-shell-command
       (concat "/var/lib/gems/1.8/bin/lessc "
               (buffer-file-name)
               " "
               ; destination
               (replace-regexp-in-string "/less/"
                                         "/css/"
                                         (replace-regexp-in-string
                                          "\.less$"
                                          "\.css"
                                          (buffer-file-name))))
       nil nil)))

(global-set-key (kbd "<f10>") 'compile-less-css)
(setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))

;; font
;(set-default-font "Envy Code R:pixelsize=15:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")
(set-default-font "-xos4-terminus-medium-r-normal-*-16-*-*-*-*-*-*-1")
;(set-default-font  "Inconsolata:pixelsize=16:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")
(setq ns-use-system-highlight-color nil)
(setq ns-pop-up-frames nil)
(global-font-lock-mode 1)

;; theme (requires 256 color term or X)
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-tangotango)
;(color-theme-twilight)

;; buffers & files navigation
(ido-mode t) ; use 'buffer rather than t to use only buffer switching
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
(add-to-list 'file-cache-filter-regexps "\\.(pyc|jpg|png|gif)" "\\.svn/.*$")
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
(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
(global-set-key (kbd "\C-x f") 'file-cache-ido-find-file)

(require 'recentf)
(setq recentf-max-saved-items 100)

;; kb shortcuts
(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "<f4>") 'start-or-end-kbd-macro)
(global-set-key (kbd "<f5>") 'call-last-macro-kbd)

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

;; not really used anymore
 (global-set-key (kbd "M-<up>") 'backward-paragraph)
 (global-set-key (kbd "M-<down>") 'forward-paragraph)

;; (defvar no-easy-keys-minor-mode-map (make-keymap)
;;   "no-easy-keys-minor-mode keymap.")
;; (let ((f (lambda (m)
;;            `(lambda () (interactive)
;;               (message (concat "No! use " ,m " instead."))))))
;;   (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
;;                ("<down>" . "C-n")
;;                ("<C-left>" . "M-f") ("<C-right>" . "M-b") ("<C-up>" . "M-{")
;;                ("<C-down>" . "M-}")
;;                ("<M-left>" . "M-f") ("<M-right>" . "M-b") ("<M-up>" . "M-{")
;;                ("<M-down>" . "M-}")
;;                ("<delete>" . "C-d") ("<C-delete>" . "M-d")
;;                ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
;;                ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
;;                ("<home>" . "C-a") ("<C-home>" . "M->")
;;                ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
;;     (define-key no-easy-keys-minor-mode-map
;;       (read-kbd-macro (car l)) (funcall f (cdr l)))))
;; (define-minor-mode no-easy-keys-minor-mode
;;   "A minor mode that disables the arrow-keys, pg-up/down, delete
;;   and backspace."  t " no-easy-keys"
;;   'no-easy-keys-minor-mode-map :global t)
;; (no-easy-keys-minor-mode 1)

;; make C-c C-c and C-c C-u work for comment/uncomment region in all modes
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; macros
(defun start-or-end-kbd-macro ()
  "Starts recording a keyboard macro, or if already recording,
   stops recording it."
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

;; full-screen
(defun toggle-fullscreen ()
(interactive)
(set-frame-parameter
 nil
 'fullscreen
 (if (frame-parameter nil 'fullscreen)
     nil
   'fullboth)))

;; nice little alternative visual bell.
;; From Miles Bader <miles /at/ gnu.org>
(defcustom mode-line-bell-string ""
  "Message displayed in mode-line by `mode-line-bell' function."
  :group 'user)
(defcustom mode-line-bell-delay 0.1
  "Number of seconds `mode-line-bell' displays its message."
  :group 'user)

(defvar mode-line-bell-cached-string nil)
(defvar mode-line-bell-propertized-string nil)
(defun mode-line-bell ()
  (unless (equal mode-line-bell-string mode-line-bell-cached-string)
    (setq mode-line-bell-propertized-string
          (propertize
           (concat
	    (propertize
	     "x"
	     'display
	     `(space :align-to (- right ,(string-width mode-line-bell-string))))
	    mode-line-bell-string)
           'face '(:background "#888888")))
    (setq mode-line-bell-cached-string mode-line-bell-string))
  (message mode-line-bell-propertized-string)
  (sit-for mode-line-bell-delay)
  (message ""))

(setq ring-bell-function 'mode-line-bell)

;; Turn off unncessary ui stuff
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; utf8 only
(setq current-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; highlight the region between the mark and point
(transient-mark-mode t)

;; dont display cursor in non selected windows
(setq-default cursor-in-non-selected-windows nil)

;; poor mans paredit
(show-paren-mode t)
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

;; 4 tabs on this project + auto less compile
(defun hw-setup ()
  (interactive)
  (add-hook 'html-mode-hook
            (lambda ()
              ;; Default indentation is usually 2 spaces, changing to 4.
              (set (make-local-variable 'sgml-basic-offset) 4)))
  (add-hook 'after-save-hook 'compile-less-css))

;; set style to hw
(hw-setup)

;; hassle free indent
(defun my-unindent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) (- tab-width))
  (setq mark-active t deactivate-mark nil))

(global-set-key (kbd "C-M-q") 'my-unindent)
(global-set-key (kbd "<C-M-tab>") 'my-unindent)

(defun my-indent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) tab-width)
  (setq mark-active t deactivate-mark nil))

(global-set-key (kbd "C-q") 'my-indent)
(global-set-key (kbd "<C-tab>") 'my-indent)

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

;; save hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't save emacs session
(setq save-place nil)

;; simple project management
(defun project-path-prompt (path)
  (interactive (list
                (ido-read-directory-name
                 "Project root: ")))
  (load-project path))

(defun load-project (path)
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find path))

(global-set-key (kbd "<f12>") 'project-path-prompt)

(setq projects (list "~/gitmu/ezads/"))

(loop for project in projects
      do (file-cache-add-directory-using-find project))
