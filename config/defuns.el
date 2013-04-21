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

;; macros
(defun start-or-end-kbd-macro ()
  "Starts recording a keyboard macro, or if already recording,
   stops recording it."
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))
