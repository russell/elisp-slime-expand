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

;; This file contains the tests.

;;; Code:

(defmacro el-slime-test-macro (a b)
  (list 'success a b))

(ert-deftest el-slime-expand-test-normal ()
  "Test all the possible operator separators."
  (with-temp-buffer
    (insert "(el-slime-test-macro 'one 'two)")
    (goto-char (point-min))
    (macroexpand-at-point)
    (save-excursion
        (switch-to-buffer macroexpand-buffer)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "(success 'one 'two)\n")))))
