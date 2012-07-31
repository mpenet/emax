;; global bindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)

(global-set-key (kbd "C-z")   'undo)
(global-set-key (kbd "<f4>") 'start-or-end-kbd-macro)
(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c m") 'execute-extended-command)

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

(global-set-key (kbd "C-.") 'find-tag)
(global-set-key (kbd "C-,") 'pop-tag-mark)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
