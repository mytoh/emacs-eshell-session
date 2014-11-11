;;; eshell-session.el --- Switch eshell buffers -*- lexical-binding: t -*-

;; Author: mytoh
;; URL: https://github.com/mytoh/emacs-eshell-session
;; Package-Requires ((emacs "24.4"))

;;; Commentary:

;; example configuration
;;
;; (require 'eshell-session)
;;
;; (define-key global-map (kbd "C-z") (make-sparse-keymap))
;; (define-key global-map (kbd "C-z C-z") 'eshell-session:switch)
;; (defun my-eshell-mode-hook ()
;;   (define-key eshell-mode-map (kbd "C-z C-n") 'eshell-session:next)
;;   (define-key eshell-mode-map (kbd "C-z C-p") 'eshell-session:prev)
;;   (define-key eshell-mode-map (kbd "C-z C-c") 'eshell-session:new))
;; (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eshell)

(defvar eshell-session:session-list nil)

(defcustom eshell-session:buffer-name eshell-buffer-name
  "default basename for buffer name"
  :group 'eshell)

;;;; funcs

(cl-defun eshell-session:default-buffer-name-p (name)
  (cl-equalp eshell-session:buffer-name name))

(cl-defun eshell-session:mode-p (mode)
  (string-equal "eshell-mode" mode))

(cl-defun eshell-session:make-buffer-name (name suffix)
  (if (cl-equalp "*" (cl-subseq name -1))
      (cl-letf ((name-but-last (string-remove-suffix "*" name)))
        (format "%s %s*" name-but-last suffix))
    (format "%s %s" name suffix)))

(cl-defun eshell-session:buffer-exists (bufname)
  (cl-find-if (lambda (buf)
                (cl-equalp (buffer-name buf) bufname))
              (buffer-list)))

(cl-defun eshell-session:buffer-name-next (name)
  (cond
    ((not name)
     eshell-session:buffer-name)
    ((eshell-session:default-buffer-name-p name)
     (eshell-session:make-buffer-name eshell-session:buffer-name
                                      "<1>"))
    (t
     (cl-letf* ((num-char (eshell-session:buffer-number name))
                (next-num-char (number-to-string (+ 1 (string-to-int num-char))))
                (suffix (cl-concatenate 'string
                                        "<" next-num-char ">")))
       (eshell-session:make-buffer-name eshell-session:buffer-name
                                        suffix)))))

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
           (string-match "<\\([0-9]+\\)>" name)
           (match-string 1 name)))))

(cl-defun eshell-session:buffer-last ()
  (cl-nth-value (- (length eshell-session:session-list) 1)
                eshell-session:session-list))

(cl-defun eshell-session:exit-hook ()
  (cl-letf ((buf (buffer-name (current-buffer))))
    (setq eshell-session:session-list
          (cl-remove buf eshell-session:session-list :test 'equal))))
(add-hook 'eshell-exit-hook
          'eshell-session:exit-hook)


(defun eshell-session:eshell (&optional arg)
  "Create an interactive Eshell buffer.
The buffer used for Eshell sessions is determined by the value of
`eshell-buffer-name'.  If there is already an Eshell session active in
that buffer, Emacs will simply switch to it.  Otherwise, a new session
will begin.  A numeric prefix arg (as in `C-u 42 M-x eshell RET')
switches to the session with that number, creating it if necessary.  A
nonnumeric prefix arg means to create a new session.  Returns the
buffer selected (or created)."
  (interactive "P")
  (cl-assert eshell-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create
                     (eshell-session:make-buffer-name eshell-session:buffer-name
                                                      (cl-concatenate 'string
                                                                      "<" (number-to-string arg) ">"))))
                   (arg
                    (generate-new-buffer eshell-session:buffer-name))
                   (t
                    (get-buffer-create eshell-session:buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

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
           (eshell-session:eshell num)))))

;;; provide
(provide 'eshell-session)

;;; eshell-session.el ends here
