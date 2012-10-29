(provide 'utils)

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
