;;; live-completions.el --- Live updating of the *Completions* buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.3
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

There is also a command `live-completions-set-columns' that you
might want to bind to a key in `minibuffer-local-completion-map'
to toggle between single and multiple column views."
  :type '(choice
          (const :tag "Single column" single)
          (const :tag "Multiple columns" multiple))
  :group 'live-completions)

(defface live-completions-forceable-candidate
  '((default :weight bold)
    (((class color) (min-colors 88) (background dark)) :background "#10104f")
    (((class color) (min-colors 88) (background light)) :background "#c4ffe0")
    (t :foreground "blue"))
  "Face for the candidate that force-completion would select."
  :group 'live-completions)

(defcustom live-completions-sort-order 'display
  "Sort order for completion candidates.
The valid choices are:

- `display': what the completions buffer traditionally uses.
  This order uses the `display-sort-function' key of the
  completion metadata and absent that, alphabetical order.

- `cycle': the order `minibuffer-force-complete' (\"tab
  cycling\") and icomplete use.  This order uses the
  `cycle-sort-function' key of the completion metadata and absent
  that it uses an order that puts the default first, recently
  used items close to the top of the list, and shorter
  candidates before longer ones.

- nil: this disables all sorting, even if the metadata specifies
  a `display-sort-function' or `cycle-sort-function'."
  :type '(choice (const display) (const cycle) (const nil))
  :group 'live-completions)

(defvar live-completions--livep nil
  "Should we continously update the *Completions* buffer?")

(defvar live-completions-columns)

;;;###autoload
(defun live-completions-set-columns (columns)
  "Set how many COLUMNS of completion candidates are displayed.

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
  (when (eq columns 'toggle)
    (setq columns
          (if (eq live-completions-columns 'single) 'multiple 'single)))
  (when (eq columns 'single)
    ;; enable standalone use, without live-completions-mode
    (advice-add 'completion--insert-strings :around
                #'live-completions--single-column '((depth . 1))))
  (let ((changep (not (eq columns live-completions-columns))))
    (setq live-completions-columns columns)
    (when (and changep (bound-and-true-p live-completions-mode))
      (live-completions--update))))

(defun live-completions--request (&rest _)
  "Request live completion."
  (setq live-completions--livep 'please))

(defun live-completions--confirm (&rest _)
  "Enable live completion if and only if a request was made."
  (setq live-completions--livep (eq live-completions--livep 'please)))

(defun live-completions--unsorted-table (&optional table)
  "Return completion table with no sorting."
  (let* ((mcp minibuffer-completion-predicate)
        (mbc (minibuffer-contents))
        (mct minibuffer-completion-table)
        (metadata (cdr (completion-metadata mbc mct mcp))))
    (unless table (setq table mct))
    (lambda (string pred action)
      (if (eq action 'metadata)
          (append
           '(metadata
             (display-sort-function . identity)
             (  cycle-sort-function . identity))
           metadata)
        (complete-with-action action table string pred)))))

(defun live-completions--cycle-order-table ()
  "Return completion table for `cycle' sort order."
  (let* ((all (completion-all-sorted-completions))
         (mct minibuffer-completion-table)
         (last (last all)))
    (when last (setcdr last nil))
    (when (or (eq mct 'help--symbol-completion-table)
              (vectorp mct)
              (and (consp mct) (symbolp (car mct))))
      (setq all (mapcar #'intern all)))
    (live-completions--unsorted-table all)))

(defun live-completions--sorted-table ()
  "Return completion table for current sort order."
  (pcase live-completions-sort-order
    ('display minibuffer-completion-table)
    ('cycle (live-completions--cycle-order-table))
    ('nil (live-completions--unsorted-table))
    (_ (user-error "Unknown live-completions-sort-order"))))

(defun live-completions--update ()
  "Update the *Completions* buffer.
Meant to be added to `post-command-hook'."
  (let ((while-no-input-ignore-events '(selection-request)))
    (while-no-input
      (condition-case nil
          (save-excursion
            (goto-char (point-max))
            (let ((minibuffer-message-timeout 0)
                  (inhibit-message t)
                  (minibuffer-completion-table
                   (live-completions--sorted-table)))
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

(defun live-completions--single-column (oldfun strings)
  "Insert completion candidate STRINGS in a single column."
  (if (eq live-completions-columns 'multiple)
      (funcall oldfun strings)
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
    (insert "\n")))

;;;###autoload
(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (let ((advice-list
         '((live-completions--highlight-forceable :before
            display-completion-list)
           (live-completions--hide-first-line :before
            completion--insert-strings)
           (live-completions--single-column :around completion--insert-strings)
           (live-completions--request :before
            completing-read read-buffer kill-buffer)
           (live-completions--confirm :before
            read-string read-from-minibuffer))))
    (if live-completions-mode
        (progn
          (add-hook 'minibuffer-setup-hook #'live-completions--setup)
          (dolist (spec advice-list)
            (dolist (fn (cddr spec)) (advice-add fn (cadr spec) (car spec)))))
      (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
      (dolist (spec advice-list)
        (dolist (fn (cddr spec)) (advice-remove fn (car spec))))
      (dolist (buffer (buffer-list))
        (when (minibufferp buffer)
          (remove-hook 'post-command-hook #'live-completions--update t))))))

(defmacro live-completions-do (config &rest body)
  "Evaluate BODY with single column live completion.
The CONFIG argument should be a plist with allowed keys
`:columns', `:separator', `:height' and `:sort'.  

The `:columns' key should map to either the symbol `single' or
`multiple'.  It defaults to `live-completions-columns'.

The separator should be a string containing at least one
newline (and can have text properties to control it's display).
It defaults to `live-completions-horizontal-separator'.

The height controls the maximum height of the completions buffer,
it can be either an integer (number of lines) or a function,
called with the completions buffer as argument, that computes the
maximum height.

The `:sort' key controls the sort order.  It defaults to
`live-completions-sort-order'; see the documentation of that
variable for the possible values associated to this key."
  (declare (indent 1))
  (let ((livep (make-symbol "livep"))
        (columns (make-symbol "columns"))
        (icompletep (make-symbol "icompletep"))
        (resizep (make-symbol "resizep"))
        (sort (make-symbol "sort"))
        (cfg (lambda (key var)
               (let ((val (plist-get config key)))
                 (when val `((,var ,val)))))))
    `(let ((,livep live-completions-mode)
           (,columns live-completions-columns)
           (,icompletep (bound-and-true-p icomplete-mode))
           (,resizep temp-buffer-resize-mode)
           (,sort live-completions-sort-order))
       (unwind-protect
           (progn
             (when ,icompletep (icomplete-mode -1))
             ,@(let ((cols (plist-get config :columns)))
                 (when cols `((live-completions-set-columns ,cols))))
             (unless ,livep (live-completions-mode))
             ,@(when (plist-get config :height)
                 `((unless ,resizep (temp-buffer-resize-mode))))
             (let (,@(funcall cfg :separator
                              'live-completions-horizontal-separator)
                   ,@(funcall cfg :height 'temp-buffer-max-height)
                   ,@(funcall cfg :sort 'live-completions-sort-order))
               ,@body))
         (live-completions-set-columns ,columns)
         (unless ,livep (live-completions-mode -1))
         ,@(when (plist-get config :height)
             `((unless ,resizep (temp-buffer-resize-mode -1))))
         (when ,icompletep (icomplete-mode))))))

(provide 'live-completions)
;;; live-completions.el ends here
