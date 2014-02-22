;;; eshell-session.el -*- lexical-binding: t -*-

(defvar eshell-session:session-list nil)
(defcustom eshell-session:buffer-name eshell-buffer-name
  "default basename for buffer name")

;;;; funcs

(cl-defun eshell-session:default-buffer-name-p (name)
  (cl-equalp eshell-session:buffer-name name))

(cl-defun eshell-session:mode-p (mode)
  (string-equal "eshell-mode" mode))

(cl-defun eshell-session:buffer-exists (bufname)
  (cl-find-if (lambda (buf)
                (cl-equalp (buffer-name buf) bufname))
              (buffer-list)))

(cl-defun eshell-session:buffer-name-next (name)
  (cond
   ((not name)
    eshell-session:buffer-name)
   ((eshell-session:default-buffer-name-p name)
    (cl-concatenate 'string
                    eshell-session:buffer-name "<1>"))
   (t
    (cl-letf* ((num-char (eshell-session:buffer-number name))
               (next-num-char (number-to-string (+ 1 (string-to-int num-char)))))
      (format "%s<%s>" eshell-session:buffer-name
              next-num-char)))))

(cl-defun eshell-session:buffer-name-prev (name)
  (cond
   ((not name)
    eshell-session:buffer-name)
   ((eshell-session:default-buffer-name-p name)
    (eshell-session:buffer-last))
   (t
    (cl-letf ((num-char (eshell-session:buffer-number name)))
      (if (cl-equalp num-char "1")
          eshell-session:buffer-name
        (cl-letf ((prev-num-char (number-to-string (- (string-to-int num-char) 1))))
          (format "%s<%s>" eshell-session:buffer-name prev-num-char)))))))

(cl-defun eshell-session:find-next (name)
  (cond
   ((cl-find (eshell-session:buffer-name-next name) eshell-session:session-list)
    (eshell-session:buffer-name-next name))
   (t
    eshell-session:buffer-name)))

(cl-defun eshell-session:buffer-number (name)
  (cond ((eshell-session:default-buffer-name-p name)
         0)
        (t
         (save-match-data
           (string-match "[0-9]+" name)
           (match-string  0 name)))))

(cl-defun eshell-session:buffer-last ()
  (cl-nth-value (- (length eshell-session:session-list) 1)
                eshell-session:session-list))

(cl-defun eshell-session:exit-hook ()
  (cl-letf ((buf (buffer-name (current-buffer))))
    (setq eshell-session:session-list
          (cl-remove buf eshell-session:session-list :test 'equal))))
(add-hook 'eshell-exit-hook
          'eshell-session:exit-hook)

;;;; command

;;; switch to eshell or restore previous windows
;;; http://irreal.org/blog/?p=1742
(cl-defun eshell-session:switch ()
  "Bring up a full-screen eshell or restore previous config."
  (interactive)
  (if (eshell-session:mode-p major-mode)
      (jump-to-register :eshell-session-winconf)
    (window-configuration-to-register :eshell-session-winconf)
    (setq eshell-buffer-name eshell-session:buffer-name)
    (if (not eshell-session:session-list)
        (setq eshell-session:session-list `(,eshell-session:buffer-name)))
    (eshell)
    (delete-other-windows)))

(cl-defun eshell-session:next ()
  "jump to next eshell buffer"
  (interactive)
  (if (eshell-session:mode-p major-mode)
      (let ((next (eshell-session:buffer-name-next (buffer-name (current-buffer)))))
        (if (eshell-session:buffer-exists next)
            (switch-to-buffer next)
          (switch-to-buffer eshell-session:buffer-name)))
    (message "Not eshell buffer")))

(cl-defun eshell-session:prev ()
  (interactive)
  (if (eshell-session:mode-p major-mode)
      (cl-letf ((prev (eshell-session:buffer-name-prev (buffer-name (current-buffer)))))
        (if (eshell-session:buffer-exists prev)
            (switch-to-buffer prev)
          (switch-to-buffer eshell-session:buffer-name)))
    (message "not eshell buffer")))

(cl-defun eshell-session:new ()
  (interactive)
  (cond ((not eshell-session:session-list)
         (eshell-session:switch))
        (t
         (cl-letf* ((next (eshell-session:buffer-name-next
                           (eshell-session:buffer-last)))
                    (num (string-to-int (eshell-session:buffer-number
                                         next))))
           (setq eshell-session:session-list
                 (cl-concatenate 'list
                                 eshell-session:session-list (list next)))
           (eshell num)))))

;;; provide
(provide 'eshell-session)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
