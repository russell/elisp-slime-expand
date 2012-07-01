;;; elisp-slime-expand.el --- A SLIME inspired expander for macros.

;; Copyright (C) 2012  Russell Sim

;; Author: Russell Sim <russell.sim@gmail.com>
;; Keywords: lisp, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode provides a similar exprience to the SLIME macroexpand
;; functions in `emacs-lisp-mode'.

;;; Code:

(defvar elisp-slime-expand-mode-map (make-sparse-keymap))

(define-key elisp-slime-expand-mode-map "\C-c\C-m"
  'macroexpand-at-point)

(define-key elisp-slime-expand-mode-map "\C-c\M-m"
  'macroexpand-all-at-point)


(define-minor-mode elisp-slime-expand-mode
  "Enable SLIME like macroexpand.

Commands:
\\{elisp-slime-expand-mode-map}"
  nil " SliExp" elisp-slime-expand-mode-map)

(defconst macroexpand-buffer "*Macroexpand*"
  "The expanded form of an sexp.")

(defvar eval-macroexpand-expression nil
  "Specifies the last macroexpansion performed. This variable
  specifies both what was expanded and how.")

(defun elisp-macro-prettyexpand (form &optional full)
  (message "Expanding...")
  (let ((cl--compiling-file full)
        (byte-compile-macro-environment nil))
    (insert (prin1-to-string
             (if full
                 (macroexpand-all form)
               (macroexpand form))))
    (message "Formatting...")
    (prog1 (pp-buffer)
      (message ""))))

(defun elisp-macroexpand-at-point-1 (&optional full)
  (save-excursion
    (let ((cpoint (point))
          (apoint (if (equal (char-to-string (char-after)) "(")
                      (point) (search-backward "(" nil t)))
          (epoint (progn (forward-sexp) (point))))
      (goto-char cpoint)
      (let ((expression (read
                         (buffer-substring-no-properties apoint epoint))))
        (when (get-buffer macroexpand-buffer)
          (kill-buffer macroexpand-buffer))
        (switch-to-buffer macroexpand-buffer)
        (when (get-buffer macroexpand-buffer)
          (set-buffer macroexpand-buffer)
          (elisp-macro-prettyexpand expression full)
          (emacs-lisp-mode)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (elisp-macroexpansion-minor-mode 1)
          (setq eval-macroexpand-expression `(,full ,expression)))))))

(defun macroexpand-all-at-point ()
  "Expand the all macros in the current sexp at point and display
it in a separate buffer."
  (interactive)
  (elisp-macroexpand-at-point-1 t))

(defun macroexpand-at-point ()
  "Expand the current sexp at point and display it in a separate
buffer."
  (interactive)
  (elisp-macroexpand-at-point-1))

(defun elisp-slime-expand-inline-at-point ()
  "Expand the current sexp if it's a macro and pretty print it."
  (interactive)
  (setq buffer-read-only nil)
  (let ((cpoint (point))
        (apoint (if (equal (char-to-string (char-after)) "(")
                    (point) (search-backward "(" nil t)))
        (epoint (progn (forward-sexp) (point))))
    (let ((expression (read (buffer-substring-no-properties apoint epoint))))
      (delete-region apoint epoint)
      (goto-char apoint)
      (elisp-macro-prettyexpand expression)
      (goto-char apoint))
    (setq buffer-read-only t)))

(defun elisp-slime-expand-macro-again ()
  "Refresh the display of the current macro."
  (interactive)
  (let ((cpoint (point)))
    (when (get-buffer macroexpand-buffer)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (elisp-macro-prettyexpand (cadr eval-macroexpand-expression)
                                (car eval-macroexpand-expression))
      (setq buffer-read-only t)
      (goto-char cpoint))))

(defvar elisp-slime-expand-minor-mode-map (make-sparse-keymap))

(define-key elisp-slime-expand-minor-mode-map "g" 'elisp-slime-expand-macro-again)
(define-key elisp-slime-expand-minor-mode-map "\C-c\C-m" 'elisp-slime-expand-inline-at-point)
(define-key elisp-slime-expand-minor-mode-map "q" 'bury-buffer)

(define-minor-mode elisp-macroexpansion-minor-mode
  "elisp mode for macroexpansion

Commands:
\\{elisp-slime-expand-minor-mode-map}"
  nil
  " Macroexpand"
  elisp-slime-expand-minor-mode-map
  (make-variable-buffer-local 'eval-macroexpand-expression))

(provide 'elisp-slime-expand)
;;; elisp-slime-expand.el ends here
