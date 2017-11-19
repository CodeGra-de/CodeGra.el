;;; codegra.el --- Give and read feedback on CodeGra.de -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Thomas Schaper and contributors
;;
;; Author: Thomas Schaper <thomas@libremail.nl> and contributors
;; URL: http://github.com/nonsequitur/smex/
;; Package-Requires: ((emacs "25"))
;; Version: 0.1.0
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;;

;;; Code:

(require 'switch-buffer-functions)
(require 'json)
(require 'cl-lib)

(defgroup codegrade nil
  "Give and read feedback on CodeGra.de"
  :group 'grading
  :link '(emacs-library-link :tag "Lisp File" "codegra.el"))

(define-derived-mode codegrade-rubric-mode markdown-mode "CodeGrade Rubric"
  "Major mode for CodeGrade rubrics."
  :group 'codegrade
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (setq-local line-move-visual t) ; see #1771
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

(lexical-let ((bindings
               '(("q" normal codegrade-rubric-close)
                 ("c" normal codegrade-toggle-rubric-item)
                 ("," normal codegrade-toggle-rubric-item)
                 ("N" normal codegrade-goto-next-header)
                 ("P" normal codegrade-goto-prev-header)
                 ("n" normal codegrade-goto-next-item)
                 ("p" normal codegrade-goto-prev-item))))
  (defvar codegrade-rubric-mode-map
    (let ((map (make-sparse-keymap)))
      (suppress-keymap codegrade-rubric-mode-map t)
      (dolist (bind bindings)
        (define-key map (kbd (car bind)) (caddr bind)))
      (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
      (define-key map [remap evil-next-line] 'evil-next-visual-line)
      map))
  (with-eval-after-load 'evil
    (dolist (bind bindings)
      (evil-define-key* (cadr bind) codegrade-rubric-mode-map
                        (kbd (car bind)) (caddr bind)))))


(define-derived-mode codegrade-feedback-mode text-mode "CodeGrade Feedback"
  "Major mode for CodeGrade feedback"
  :group 'codegrade
  (visual-line-mode 1)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

(defvar codegrade-feedback-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'codegrade-feedback-close)
    (define-key map [remap server-edit] 'codegrade-feedback-close)
    (define-key map [remap evil-save-and-close] 'codegrade-feedback-close)
    (define-key map [remap evil-save-modified-and-close] 'codegrade-feedback-close)
    (define-key map (kbd "C-c C-k") 'codegrade-feedback-quit)
    (define-key map [remap kill-buffer] 'codegrade-feedback-quit)
    (define-key map [remap ido-kill-buffer] 'codegrade-feedback-quit)
    (define-key map [remap iswitchb-kill-buffer] 'codegrade-feedback-quit)
    (define-key map [remap evil-quit] 'codegrade-feedback-quit)
    map))

(defvar codegrade-rubric-buffer nil
  "The rubric buffer that is open at the moment or NIL.")

(defvar-local codegrade--feedback-previous-window-conf nil
  "Internal use.")
(defvar-local codegrade--feedback-line nil
  "Internal use.")
(defvar-local codegrade--feedback-filename nil
  "Internal use.")

(defvar codegrade-rubric-file nil
  "The rubric file that is open at the moment or NIL.")

(defvar codegrade-rubric-previous-window-conf nil
  "The previous window configuration.")

(defvar-local codegrade-buffer-p 'NONE
  "Is this buffer a buffer in the codegra.fs filesystem")

;;; Helper functions

(defun codegrade-handle-api-errors (proc)
  "Handle all errors from a codegrade api consumer process PROC."
  (case (process-exit-status proc)
    (0 nil)
    (3 (error "Socket file not found, are you in codegra.fs?"))
    (otherwise (error "Error (%s): %S"
                      (process-exit-status proc)
                      (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defmacro codegrade-with-process (given-name (program &rest args) cont)
  (declare (indent 2))
  (let ((name (cl-gensym))
        (buf (cl-gensym))
        (started-proc (cl-gensym)))
    `(progn
       (lexical-let* ((,name ,given-name)
                      (,buf (generate-new-buffer (concat "*" ,name "*")))
                      (,started-proc (let ((process-connection-type nil))
                                       (start-process ,name ,buf ,program ,@args))))
         (with-current-buffer ,buf
           (set-process-sentinel ,started-proc
                                 (lambda (proc &optional _)
                                   (when (eql 'exit (process-status proc))
                                     (with-current-buffer (process-buffer proc)
                                       (unwind-protect
                                           (funcall ,cont proc)
                                         (kill-buffer (current-buffer))))))))
         nil))))

(defun codegrade-find-special-file (current-file wanted-file-name)
  "Find a special file in the codegra.fs filesystem.

Starting from root to the containing directory of CURRENT-FILE a file
named WANTED-FILE-NAME is searched.  It will return its containing
directory if found, otherwise NIL."
  (block 'func
    (let* ((total-path ""))
      (assert current-file)
      (dolist (part (split-string current-file "/"))
        (unless (string= part "")
          (setq total-path (format "%s/%s" total-path part))
          (when (and (file-directory-p total-path)
                     (file-exists-p (format "%s/%s" total-path wanted-file-name)))
            (return-from 'func
              (format "%s/%s" total-path wanted-file-name))))))))

(defun codegrade-find-rubric-file (file)
  "Search for the rubric file bellowing to FILE.

See CODEGRADE-FIND-SPECIAL for how this is done."
  (codegrade-find-special-file file ".cg-rubric.md"))

(defun codegrade-find-grade-file (file)
  "Search for the grade file bellowing to FILE.

See CODEGRADE-FIND-SPECIAL for how this is done."
  (codegrade-find-special-file file ".cg-grade"))

(defun codegrade--get-json-feedback (cont)
  "Get feedback for the current file on the current line.

The continuation CONT is called with the parsed json as the feedback
  on the current line."
  (codegrade-with-process "codegrade-api-consumer"
      ("cgapi-consumer"
       "get-comment"
       (buffer-file-name))
    (lambda (proc)
      (goto-char (point-min))
      (codegrade-handle-api-errors proc)
      (let ((json (json-read)))
        (funcall cont json)))))

(defun codegrade-ensure-mode (mode)
  "Ensure that the current major mode is MODE."
  (unless (eql major-mode mode)
    (error "The current mode is not %s as required for this command" mode)))

;;; End helper functions

(defun codegrade-rubric-close (&optional not-kill)
  "Close the current rubric buffer.

If NOT-KILL is non nil the current CODEGRADE-RUBRIC-BUFFER is not
  killed but only some cleanup is done.  Otherwise it is killed."
  (interactive)
  (unless not-kill
    (kill-buffer codegrade-rubric-buffer)
    (when codegrade-rubric-previous-window-conf
      (set-window-configuration codegrade-rubric-previous-window-conf)
      (setq codegrade-rubric-previous-window-conf nil)))
  (setq codegrade-rubric-file nil)
  (setq codegrade-rubric-buffer nil))

(defun codegrade-feedback-quit ()
  "Stop giving feedback.

This does not save the currently typed in feedback."
  (interactive)
  (codegrade-ensure-mode 'codegrade-feedback-mode)

  (kill-buffer (current-buffer))
  (set-window-configuration codegrade--feedback-previous-window-conf)
  (message "Feedback quited!"))

(defun codegrade-feedback-close ()
  "Close the current feedback buffer and send its content as feedback."
  (interactive)
  (codegrade-ensure-mode 'codegrade-feedback-mode)

  (lexical-let ((buf (current-buffer)))
    (whitespace-cleanup-region (point-min) (point-max))
    (codegrade-with-process "codegrade-api-consumer"
        ("cgapi-consumer"
         "set-comment"
         codegrade--feedback-filename
         (int-to-string codegrade--feedback-line)
         (buffer-substring-no-properties (point-min) (point-max)))
      (lambda (proc)
        (codegrade-handle-api-errors proc)

        (with-current-buffer buf
          (set-window-configuration codegrade--feedback-previous-window-conf)
          (kill-buffer buf)
          (message "Feedback added!"))))))

(defun codegrade-goto-next-header ()
  "Goto the next rubric header."
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (condition-case nil
        (progn (search-forward-regexp "^## ")
               (beginning-of-line))
      (error
       (goto-char pt)
       (error "No next item found, you are on the last item")))))

(defun codegrade-goto-prev-header ()
  "Goto the previous rubric header."
  (interactive)
  (condition-case nil
      (progn (search-backward-regexp "## ")
             (beginning-of-line))
    (error (error "No previous item found, you are on the first item"))))

(defun codegrade-goto-prev-item ()
  "Goto the previous rubric item.

A rubric items is header or a list item."
  (interactive)
  (condition-case nil
      (progn (search-backward-regexp "^\\(## \\|- \\[.\\] \\)")
             (beginning-of-line))
    (error (error "No previous item found, you are on the first item"))))

(defun codegrade-goto-next-item ()
  "Goto the next rubric item.

A rubric items is header or a list item."
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (condition-case nil
        (progn (search-forward-regexp "^\\(## \\|- \\[.\\] \\)")
               (beginning-of-line))
      (error
       (goto-char pt)
       (error "No next item found, you are on the last item")))))

(defun codegrade-toggle-rubric-item ()
  "Toggle the rubric list item at the current point.

This looks back to find the first line starting with \"- []\" or \"-
[x]\".  If it finds a description seperator line or a rubric header
before that it will error."
  (interactive)
  (codegrade-ensure-mode 'codegrade-rubric-mode)
  
  (let ((buffer-read-only nil))
    (unless (equal (current-buffer) codegrade-rubric-buffer)
      (error "This command only works in the codegrade rubric buffer!"))
    (unless codegrade-rubric-file
      (error "No codegrade rubric file is open!"))

    (save-excursion
      (beginning-of-line)
      (while (and (not (or (looking-at "^- \\[.\\] ")
                           (looking-at "^-------*$")
                           (looking-at "^## ")))
                  (= (forward-line -1) 0)))
      (cond
       ((looking-at "^- \\[ \\] ")
        (let ((cur-pt (point)))
          (search-backward-regexp "^## " nil 'noerror)
          (beginning-of-line)
          (while (and (= (forward-line 1) 0)
                      (not (looking-at "^## ")))
            (when (looking-at "^- \\[x\\] ")
              (replace-match "- [ ] ")))
          (goto-char cur-pt)
          (assert (looking-at "^- \\[ \\] "))
          (replace-match "- [x] ")))
       ((looking-at "^- \\[x\\] ")
        (replace-match "- [ ] "))
       (t (error "Not on a rubric item!"))))

    (write-region nil nil codegrade-rubric-file)
    (insert-file-contents codegrade-rubric-file nil nil nil 'replace)
    (set-buffer-modified-p nil)))

;;;###autoload
(defun codegrade-open-rubric ()
  "Open or switch to the codegrade rubric buffer.

The rubric for the current buffer will be loaded in this codegrade
rubric buffer and using this buffer you can fill the rubric in.  This
buffer will automatically change the rubric for the currently focused
file."
  (interactive)
  (if codegrade-rubric-buffer
      (switch-to-buffer-other-window codegrade-rubric-buffer)
    (let ((old-file (buffer-file-name (current-buffer)))
          (buf (generate-new-buffer "*codegrade-rubric*")))
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (interactive)
                    (codegrade-rubric-close 'not-kill))
                  'not-append
                  'local)
        (let ((f (codegrade-find-rubric-file old-file)))
          (if f
              (progn
                (insert-file-contents f nil nil nil 'replace)
                (setq codegrade-rubric-file f))
            (message "Rubric not found, leaving buffer empty!")))
        (message "Setting buf to %S" buf)
        (setq codegrade-rubric-buffer buf)
        (codegrade-rubric-mode)
        (setq codegrade-rubric-previous-window-conf (current-window-configuration))
        (switch-to-buffer-other-window buf)))))

(defun codegrade-rubric-hook (_ cur)
  "The rubric hook that is run everytime a buffer is changed.

It checks if there is a rubric buffer open and if it is it will update
its contents if the newly focused file (which is CUR) is a codegrade fill."
  (ignore-errors
    (when (and codegrade-rubric-buffer
               (not (eql major-mode 'codegrade-rubric-mode))
               (buffer-file-name cur))
      (let ((f nil))
        (when (not (fboundp 'codegrade-buffer-p))
          (setq f (codegrade-find-rubric-file (buffer-file-name cur)))
          (setq-local codegrade-buffer-p (and f t)))
        (when (and codegrade-buffer-p
                   (not (string= (or f
                                     (codegrade-find-rubric-file (buffer-file-name cur)))
                                 codegrade-rubric-file)))
          (message "New student detected!")
          (setq codegrade-rubric-file f)
          (with-current-buffer codegrade-rubric-buffer
            (let ((buffer-read-only nil))
              (insert-file-contents codegrade-rubric-file nil nil nil 'replace)
              (goto-char (point-min)))))))))

(add-hook 'switch-buffer-functions #'codegrade-rubric-hook)

;;;###autoload
(defun codegrade-add-feedback ()
  "Add feedback for the current file on the current line."
  (interactive)
  (lexical-let ((line (line-number-at-pos))
                (name (buffer-file-name)))
    (codegrade--get-json-feedback
     (lambda (json)
       (lexical-let ((old-conf (current-window-configuration))
                     (buf (generate-new-buffer "*codegrade-feedback*"))
                     (feedback (cl-find-if (lambda (el)
                                             (= (cdr (assoc 'line el))
                                                line))
                                           json)))
         (with-current-buffer buf
           (when feedback
             (insert (cdr (assoc 'content feedback))))
           (goto-char (point-min))
           (codegrade-feedback-mode)
           (switch-to-buffer-other-window buf)
           (setq-local codegrade--feedback-filename name)
           (setq-local codegrade--feedback-previous-window-conf old-conf)
           (setq-local codegrade--feedback-line line)
           (message "Type your feedback here")))))))

;;;###autoload
(defun codegrade-give-grade (grade)
  "Give the current submission the grade GRADE."
  (interactive "nGrade: ")
  (when (or (< grade 0) (> grade 10))
    (error "Grade should be between 0 and 10"))
  (let ((current (buffer-file-name)))
    (with-temp-buffer
      (insert (format "%s" grade))
      (write-region nil nil (codegrade-find-grade-file current))
      (message "Grade given!"))))

;;;###autoload
(defun codegrade-delete-feedback ()
  "Delete the feedback for the current file on the current line."
  (interactive)
  (lexical-let ((line (line-number-at-pos)))
    (codegrade-with-process "codegrade-api-consumer"
        ("cgapi-consumer"
         "delete-comment"
         (buffer-file-name)
         (int-to-string line))
      (lambda (proc)
        (codegrade-handle-api-errors proc)

        (message "Feedback deleted!")))))

;;;###autoload
(defun codegrade-get-feedback ()
  "Get feedback for the current file on the current line.

This feedback is messaged back to the user."
  (interactive)
  (lexical-let ((line (line-number-at-pos)))
    (codegrade--get-json-feedback
     (lambda (json)
       (message "%S" (cl-find-if (lambda (el)
                                   (= (cdr (assoc 'line el))
                                      line))
                                 json))))))

(provide 'codegra)

;;; codegra.el ends here
