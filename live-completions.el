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

(eval-when-compile (require 'subr-x))

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
    (((class color) (min-colors 88) (background dark)) :background "#010a5f")
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
  :type '(choice (const :tag "Classic *Completions*" display)
                 (const :tag "Tab cycling or Icomplete order" cycle)
                 (const :tag "No sorting" nil))
  :group 'live-completions)

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

(defun live-completions--unsorted-table ()
  "Return completion table with no sorting."
  (let* ((mbc (minibuffer-contents))
         (mct minibuffer-completion-table)
         (mcp minibuffer-completion-predicate)
         (metadata (cdr (completion-metadata mbc mct mcp))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          (append
           '(metadata
             (display-sort-function . identity)
             (  cycle-sort-function . identity))
           metadata)
        (complete-with-action action mct string pred)))))

(defun live-completions--cycle-order-table ()
  "Return completion table for `cycle' sort order."
  (let* ((mbc (minibuffer-contents))
         (mct minibuffer-completion-table)
         (mcp minibuffer-completion-predicate)
         (metadata (cdr (completion-metadata mbc mct mcp)))
         (first-pass
          (or
           (completion-metadata-get metadata 'cycle-sort-function)
           (lambda (all)
             (sort all (lambda (c1 c2) (< (length c1) (length c2)))))))
         (sorter
          (let ((hist (if (version< emacs-version "27")
                          (symbol-value minibuffer-history-variable)
                        (minibuffer-history-value))))
            (lambda (all)
              (setq all (funcall first-pass all))
              (sort all (lambda (c1 c2)
                          (> (length (member c1 hist))
                             (length (member c2 hist)))))))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          (append
           `(metadata
             (display-sort-function . ,sorter)
             (  cycle-sort-function . ,sorter))
           metadata)
        (complete-with-action action mct string pred)))))

(defun live-completions--sort-order-table ()
  "Return completion table for current sort order."
  (pcase live-completions-sort-order
    ('display minibuffer-completion-table)
    ('cycle (live-completions--cycle-order-table))
    ('nil (live-completions--unsorted-table))
    (_ (user-error "Unknown live-completions-sort-order"))))

(defun live-completions--update (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when (minibufferp) ; skip if we've exited already
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (condition-case nil
            (save-match-data
              (save-excursion
                (goto-char (point-max))
                (let ((inhibit-message t)
                      (minibuffer-completion-table
                       (live-completions--sort-order-table)))
                  (minibuffer-completion-help)
                  (run-at-time 0 nil #'live-completions--highlight-forceable))))
          (quit (abort-recursive-edit)))))))

(defun live-completions--highlight-forceable ()
  "Highlight the completion that `minibuffer-force-complete' would insert."
  (when-let ((first (car (completion-all-sorted-completions))))
    (with-current-buffer "*Completions*"
      (goto-char (point-min))
      (let (donep)
        (while (not donep)
          (next-completion 1)
          (if (eobp)
              (setq donep t)
            (let* ((beg (point))
                   (end (or (next-single-property-change beg 'mouse-face)
                            (point-max))))
              (when (string= (buffer-substring beg end) first)
                (let ((inhibit-read-only t))
                  (font-lock-prepend-text-property
                   beg end
                   'face 'live-completions-forceable-candidate))
                (setq donep t)))))))))

(defun live-completions--honor-inhibit-message (fn &rest args)
  "Skip applying FN to ARGS if inhibit-message is t.
Meant as `:around' advice for `minibuffer-message', which does
not honor minibuffer message."
  (unless inhibit-message
    (apply fn args)))

(defun live-completions--setup ()
  "Setup live updating for the *Completions* buffer.
Meant to be added to `minibuffer-setup-hook'."
  (remove-hook 'minibuffer-setup-hook #'live-completions--setup)
  (setq-local completion-show-inline-help nil)
  (add-hook 'after-change-functions #'live-completions--update nil t)
  (run-with-idle-timer 0.01 nil #'live-completions--update))

(defun live-completions--first-completion ()
  "When at the start of the completions buffer go to first completion."
  (when (bobp) (next-completion 1)))

(defun live-completions--hide-help ()
  "Make help message in *Completions* buffer invisible.
Meant to be add to `completion-setup-hook'."
  (with-current-buffer standard-output
    (goto-char (point-min))
    (next-completion 1)
    (put-text-property (point-min) (point) 'invisible t)
    (add-hook 'pre-command-hook #'live-completions--first-completion 5 t)))

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

(defun live-completions-read
    (prompt collection &optional predicate
            require-match initial-input hist def inherit-input-method)
  "Read with live updating of the completions buffer.
To be used as a `completing-read-function'.  For the meanings of
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'."
  (condition-case err
      (progn
        (add-hook 'minibuffer-setup-hook #'live-completions--setup)
        (completing-read-default        ; not live-completions--old-crf!
         prompt collection predicate
         require-match initial-input hist def inherit-input-method))
    (t (signal (car err) (cdr err)))))

(defvar live-completions--old-crf nil
  "Store previous value of `completing-read-function'")

;;;###autoload
(define-minor-mode live-completions-mode
  "Live updating of the *Completions* buffer."
  :global t
  (let ((advice-list
         '((completion--insert-strings
            :around live-completions--single-column)
           (minibuffer-message
            :around live-completions--honor-inhibit-message)
           (completion--flush-all-sorted-completions
            :after live-completions--update))))
    (if live-completions-mode
        (progn
          (setq live-completions--old-crf completing-read-function
                completing-read-function #'live-completions-read)
          (add-hook 'completion-setup-hook #'live-completions--hide-help 1)
          (dolist (spec advice-list) (apply #'advice-add spec)))
      (when live-completions--old-crf
        (setq completing-read-function live-completions--old-crf
              live-completions--old-crf nil))
      (remove-hook 'completion-setup-hook #'live-completions--hide-help)
      (dolist (spec advice-list) (advice-remove (car spec) (caddr spec))))))

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
        (icompletep (make-symbol "icompletep"))
        (resizep (make-symbol "resizep"))
        (completions-height (make-symbol "completions-height"))
        (other-temp-height (make-symbol "other-temp-height"))
        (cfg (lambda (key var)
               (let ((val (plist-get config key)))
                 (when val `((,var ,val))))))
        (height (plist-get config :height)))
    (when height
      (setf config
            (plist-put
             config :height-fn ; affect only *Completions*
             (let ((buf (make-symbol "buf"))
                   (ht (make-symbol "ht")))
               `(lambda (,buf)
                  (let ((,ht (if (string= (buffer-name ,buf) "*Completions*")
                                 ,completions-height
                               ,other-temp-height)))
                    (if (functionp ,ht) (funcall ,ht ,buf) ,ht)))))))
    `(let ((,livep live-completions-mode)
           (,icompletep (bound-and-true-p icomplete-mode))
           (,resizep temp-buffer-resize-mode)
           ,@(when height
               `((,completions-height ,height) ; evaluate height only once
                 (,other-temp-height temp-buffer-max-height))))
       (unwind-protect
           (progn
             (when ,icompletep (icomplete-mode -1))
             (unless ,livep (live-completions-mode))
             ,@(when height `((unless ,resizep (temp-buffer-resize-mode))))
             (let (,@(funcall cfg :columns 'live-completions-columns)
                   ,@(funcall cfg :separator
                              'live-completions-horizontal-separator)
                   ,@(funcall cfg :sort 'live-completions-sort-order)
                   ,@(funcall cfg :height-fn 'temp-buffer-max-height))
               ,@body))
         (unless ,livep (live-completions-mode -1))
         ,@(when height `((unless ,resizep (temp-buffer-resize-mode -1))))
         (when ,icompletep (icomplete-mode))))))

(provide 'live-completions)
;;; live-completions.el ends here
