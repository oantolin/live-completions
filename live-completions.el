;;; live-completions.el --- Live updating of the *Completions* buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.2
;; Homepage: https://github.com/oantolin/live-completions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides a minibuffer completion UI.  It pops up the
;; traditional *Completions* buffer and keeps it updated as you type
;; in the minibuffer.  Besides the traditional multicolumn view of the
;; completions, this package offers a single column view ---in case
;; the changing number of columns of the traditional view proves too
;; dizzying.

;;; Code:

(defgroup live-completions nil
  "Live updating of the *Completions* buffer."
  :group 'completion)

(defcustom live-completions-horizontal-separator "\n"
  "Candidate separator for live-completions in single-column mode.
The separator should contain at least one newline."
  :type 'string
  :group 'live-completions)

(defcustom live-completions-columns 'multiple
  "How many columns of candidates live-completions displays.
To change the value from Lisp code use
`live-completions-set-columns'."
  :type '(choice
          (const :tag "Single column" single)
          (const :tag "Multiple columns" multiple))
  :set (lambda (var columns)
         (if (and (not (boundp var)) (eq columns 'multiple))
             (set var 'multiple )
           (live-completions-set-columns columns)))
  :group 'live-completions)

(defface live-completions-forceable-candidate
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :background "#10104f")
    (((class color) (min-colors 88) (background light)) :background "#c4ffe0")
    (t :foreground "blue"))
  "Face for the candidate that force-completion would select."
  :group 'live-completions)

(defvar live-completions--livep nil
  "Should we continously update the *Completions* buffer?")

(defun live-completions-set-columns (columns)
  "Set how many COLUMNS of candidates are displayed.

Called from Lisp code COLUMNS should be one of the symbols
`single', `multiple' or `toggle'.

When called interactively without prefix argument, toggle between
single and multiple columns.  Called with a numerical prefix of 1,
set single column mode, any other prefix argument sets multiple
columns."
  (interactive
   (list (pcase current-prefix-arg
           ('nil 'toggle)
           (1 'single)
           (_ 'multiple))))
  (pcase columns
    ('single
     (advice-add 'completion--insert-strings :around
                 #'live-completions--single-column '((depth . 1))))
    ('multiple
     (advice-remove 'completion--insert-strings
                    #'live-completions--single-column))
    ('toggle
     (live-completions-set-columns
      (if (advice-member-p #'live-completions--single-column
                           'completion--insert-strings)
          'multiple
        'single))))
  (when (and (bound-and-true-p live-completions-mode)
             (not (eq columns 'toggle)))
    (live-completions--update))
  (setq live-completions-columns columns))

(defun live-completions--request (&rest _)
  "Request live completion."
  (setq live-completions--livep 'please))

(defun live-completions--confirm (&rest _)
  "Enable live completion if and only if a request was made."
  (setq live-completions--livep (eq live-completions--livep 'please)))

(defun live-completions--update ()
  "Update the *Completions* buffer.
Meant to be added to `post-command-hook'."
  (let ((while-no-input-ignore-events '(selection-request)))
    (while-no-input
      (condition-case nil
          (save-excursion
            (goto-char (point-max))
            (let ((minibuffer-message-timeout 0))
              (minibuffer-completion-help)))
        (quit (abort-recursive-edit))))))

(defun live-completions--highlight-forceable (completions &optional _common)
  "Highlight the completion that `minibuffer-force-complete' would insert.
Meant to be used as advice for `display-completion-list', which
is were the COMPLETIONS argument comes from."
  (let ((first (car (member (car (completion-all-sorted-completions))
                            completions))))
    (when first
      (font-lock-prepend-text-property
       0 (length first)
       'face 'live-completions-forceable-candidate
       first))))

(defun live-completions--setup ()
  "Setup live updating for the *Completions* buffer.
Meant to be added to `minibuffer-setup-hook'."
  (when live-completions--livep
    (sit-for 0.01)
    (add-hook 'post-command-hook #'live-completions--update nil t)))

(defun live-completions--hide-first-line (&rest _)
  "Make first line in *Completions* buffer invisible."
  (when (string= (buffer-name) "*Completions*")
    (save-excursion
      (goto-char (point-min))
      (put-text-property (point) (1+ (line-end-position)) 'invisible t))))

(defun live-completions--single-column (_oldfun strings)
  "Insert completion candidate STRINGS in a single column."
  (dolist (str strings)
    (if (not (consp str))
        (put-text-property (point) (progn (insert str) (point))
                           'mouse-face 'highlight)
      (put-text-property (point) (progn (insert (car str)) (point))
                         'mouse-face 'highlight)
      (let ((beg (point))
            (end (progn (insert (cadr str)) (point))))
        (put-text-property beg end 'mouse-face nil)
        (font-lock-prepend-text-property beg end 'face
                                         'completions-annotations)))
    (insert live-completions-horizontal-separator))
  (delete-region (- (point) (length live-completions-horizontal-separator))
                 (point))
  (insert "\n"))

;;;###autoload
(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (let ((advice-list
         '((display-completion-list live-completions--highlight-forceable)
           (completion--insert-strings live-completions--hide-first-line)
           (completing-read live-completions--request)
           (read-buffer-to-switch live-completions--request)
           (read-from-minibuffer live-completions--confirm))))
    (if live-completions-mode
        (progn
          (add-hook 'minibuffer-setup-hook #'live-completions--setup)
          (dolist (advice advice-list)
            (advice-add (car advice) :before (cadr advice))))
      (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
      (dolist (advice advice-list)
        (advice-remove (car advice) (cadr advice)))
      (dolist (buffer (buffer-list))
        (when (minibufferp buffer)
          (remove-hook 'post-command-hook #'live-completions--update t))))))

(defmacro live-completions-single-column-do (separator &rest body)
  "Evaluate BODY with single column live completion.
Use SEPARATOR to separate the candidates."
  (declare (indent 1))
  (let ((livep (make-symbol "livep"))
        (columns (make-symbol "columns"))
        (icompletep (make-symbol "icompletep")))
    `(let ((,livep live-completions-mode)
           (,columns live-completions-columns)
           (,icompletep (bound-and-true-p icomplete-mode)))
       (unwind-protect
           (progn
             (when ,icompletep (icomplete-mode -1))
             (live-completions-set-columns 'single)
             (live-completions-mode)
             (let ((live-completions-horizontal-separator
                    (or ,separator live-completions-horizontal-separator)))
               ,@body))
         (live-completions-set-columns ,columns)
         (unless ,livep (live-completions-mode -1))
         (when ,icompletep (icomplete-mode))))))

(provide 'live-completions)
;;; live-completions.el ends here
