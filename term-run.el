;;; term-run.el --- Run command in terminal buffer

;; Author: 10sr <8.slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/term-run-el
;; Version: 0.1
;; Package-Requires: ((term "0"))
;; Keywords: utility shell command term-mode

;; This file is not part of GNU Emacs.

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

;; term-run.el


;;; Code:

(eval-and-compile
  (require 'term))

(defvar term-run-shell-command-history nil
  "History for `term-run-shell-command'.")

(defun term-run (program &optional buffer-or-name &rest args)
  "Run PROGRAM in BUFFER-OR-NAME with ARGS.

If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in the
buffer will be destroyed.  Otherwise, new buffer will be generated automatically
from PROGRAM.

This function returns the buffer where the process ran."
  (let* ((buf (if buffer-or-name
                  (get-buffer-create buffer-or-name)
                (generate-new-buffer (concat "*"
                                             "Term-Run "
                                             program
                                             "*"))))
         (proc (get-buffer-process buf))
         (dir default-directory))
    (and proc
         (delete-process proc))
    (display-buffer buf)
    (with-current-buffer buf
      (cd dir)
      (set (make-local-variable 'term-scroll-to-bottom-on-output)
           t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert "Start executing "
                program)
        (add-text-properties (point-at-bol)
                             (point-at-eol)
                             '(face bold))
        (insert "\n\n"))
      (term-mode)
      (term-exec buf
                 (concat "term-" program)
                 program
                 nil
                 args)
      (term-char-mode)
      (if (ignore-errors (get-buffer-process buf))
          (set-process-sentinel (get-buffer-process buf)
                                (lambda (proc change)
                                  (with-current-buffer (process-buffer proc)
                                    (term-sentinel proc change)
                                    (goto-char (point-max)))))
        ;; (goto-char (point-max))
        ))
    buf))

(defun term-run-shell-command (command &optional new-buffer-p)
  "Run COMMAND in terminal emulator.

If NEW-BUFFER-P is given or called with prefix argument, generate new terminal
buffer for running COMMAND.  Otherwise, use the same buffer.  In this case, old
process in the buffer will be destroyed."
  (interactive (list (read-shell-command "Run program: "
                                         nil
                                         'term-run-shell-command-history)
                     current-prefix-arg))
  (let ((buf (if new-buffer-p
                 (generate-new-buffer "*Term-Run Shell Command*")
               (get-buffer-create "*Term-Run Shell Command*")))
        (shell (or explicit-shell-file-name
                   shell-file-name
                   (getenv "SHELL")
                   "/bin/sh")))
    (term-run shell
              buf
              shell-command-switch
              command)))


(provide 'term-run)

;;; term-run.el ends here
