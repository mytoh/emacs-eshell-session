;;; eshell-switch.el --- Switch eshell buffers -*- lexical-binding: t -*-

;; Author: mytoh
;; URL: https://github.com/mytoh/emacs-eshell-switch
;; Package-Requires ((emacs "24.4"))

;;; Commentary:

;; example configuration
;;
;; (require 'eshell-switch)
;;
;; (define-key global-map (kbd "C-z") (make-sparse-keymap))
;; (define-key global-map (kbd "C-z C-z") 'eshell-switch:switch)
;; (defun my-eshell-mode-hook ()
;;   (define-key eshell-mode-map (kbd "C-z C-n") 'eshell-switch:buffer-next)
;;   (define-key eshell-mode-map (kbd "C-z C-p") 'eshell-switch:buffer-prev)
;;   (define-key eshell-mode-map (kbd "C-z C-c") 'eshell-switch:buffer-new))
;; (add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eshell)

(defvar eshell-switch:session-list nil)

(defcustom eshell-switch:buffer-name eshell-buffer-name
  "default basename for buffer name"
  :group 'eshell)

(cl-defun eshell-switch:default-buffer-name-p (name)
  (cl-equalp eshell-switch:buffer-name name))

(cl-defun eshell-switch:mode-p (mode)
  (string-equal "eshell-mode" mode))

(cl-defun eshell-switch:make-buffer-name (name suffix)
  (if (cl-equalp "*" (cl-subseq name -1))
      (cl-letf ((name-but-last (string-remove-suffix "*" name)))
        (format "%s %s*" name-but-last suffix))
    (format "%s %s" name suffix)))

(cl-defun eshell-switch:buffer-exists (bufname)
  (cl-find-if (lambda (buf)
                (cl-equalp (buffer-name buf) bufname))
              (buffer-list)))

(cl-defun eshell-switch:buffer-name-next (name)
  (cond
    ((not name)
     eshell-switch:buffer-name)
    ((eshell-switch:default-buffer-name-p name)
     (eshell-switch:make-buffer-name eshell-switch:buffer-name
                                     "<1>"))
    (t
     (cl-letf* ((num-char (eshell-switch:buffer-number name))
                (next-num-char (number-to-string (+ 1 (string-to-number num-char))))
                (suffix (cl-concatenate 'string
                                        "<" next-num-char ">")))
       (eshell-switch:make-buffer-name eshell-switch:buffer-name
                                       suffix)))))

(cl-defun eshell-switch:buffer-name-prev (name)
  (cond
    ((not name)
     eshell-switch:buffer-name)
    ((eshell-switch:default-buffer-name-p name)
     (eshell-switch:buffer-last))
    (t
     (cl-letf ((num-char (eshell-switch:buffer-number name)))
       (if (cl-equalp num-char "1")
           eshell-switch:buffer-name
         (cl-letf* ((prev-num-char (number-to-string (- (string-to-number num-char) 1)))
                    (suffix (cl-concatenate 'string "<" prev-num-char ">")))
           (eshell-switch:make-buffer-name eshell-switch:buffer-name
                                           suffix)))))))

(cl-defun eshell-switch:find-next (name)
  (cond
    ((cl-find (eshell-switch:buffer-name-next name) eshell-switch:session-list)
     (eshell-switch:buffer-name-next name))
    (t
     eshell-switch:buffer-name)))

(cl-defun eshell-switch:buffer-number (name)
  (cond ((eshell-switch:default-buffer-name-p name)
         0)
        (t
         (save-match-data
           (string-match "<\\([0-9]+\\)>" name)
           (match-string 1 name)))))

(cl-defun eshell-switch:buffer-last ()
  (cl-nth-value (- (length eshell-switch:session-list) 1)
                eshell-switch:session-list))

(cl-defun eshell-switch:exit-hook ()
  (cl-letf ((buf (buffer-name (current-buffer))))
    (setq eshell-switch:session-list
          (cl-remove buf eshell-switch:session-list :test 'equal))))
(add-hook 'eshell-exit-hook
          'eshell-switch:exit-hook)


(defun eshell-switch:eshell (&optional arg)
  (interactive "P")
  (cl-assert eshell-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create
                     (eshell-switch:make-buffer-name eshell-switch:buffer-name
                                                     (cl-concatenate 'string
                                                                     "<" (number-to-string arg) ">"))))
                   (arg
                    (generate-new-buffer eshell-switch:buffer-name))
                   (t
                    (get-buffer-create eshell-switch:buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

;;; switch to eshell or restore previous windows
;;; http://irreal.org/blog/?p=1742
;;;###autoload
(cl-defun eshell-switch:switch ()
  "Bring up a full-screen eshell or restore previous config."
  (interactive)
  (if (eshell-switch:mode-p major-mode)
      (jump-to-register :eshell-switch-winconf)
    (window-configuration-to-register :eshell-switch-winconf)
    (setq eshell-buffer-name eshell-switch:buffer-name)
    (if (not eshell-switch:session-list)
        (setq eshell-switch:session-list `(,eshell-switch:buffer-name)))
    (eshell)
    (delete-other-windows)))

;;;###autoload
(cl-defun eshell-switch:buffer-next ()
  "jump to next eshell buffer"
  (interactive)
  (if (eshell-switch:mode-p major-mode)
      (let ((next (eshell-switch:buffer-name-next (buffer-name (current-buffer)))))
        (if (eshell-switch:buffer-exists next)
            (switch-to-buffer next)
          (switch-to-buffer eshell-switch:buffer-name)))
    (message "Not eshell buffer")))

;;;###autoload
(cl-defun eshell-switch:buffer-prev ()
  (interactive)
  (if (eshell-switch:mode-p major-mode)
      (cl-letf ((prev (eshell-switch:buffer-name-prev (buffer-name (current-buffer)))))
        (if (eshell-switch:buffer-exists prev)
            (switch-to-buffer prev)
          (switch-to-buffer eshell-switch:buffer-name)))
    (message "not eshell buffer")))

;;;###autoload
(cl-defun eshell-switch:buffer-new ()
  (interactive)
  (cond ((not eshell-switch:session-list)
         (eshell-switch:switch))
        (t
         (cl-letf* ((next (eshell-switch:buffer-name-next
                           (eshell-switch:buffer-last)))
                    (num (string-to-number (eshell-switch:buffer-number
                                            next))))
           (setq eshell-switch:session-list
                 (cl-concatenate 'list
                                 eshell-switch:session-list (list next)))
           (eshell-switch:eshell num)))))

(provide 'eshell-switch)

;;; eshell-switch.el ends here
