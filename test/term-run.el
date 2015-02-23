(require 'term-run)

(defmacro ert-term-run-make-output (cmds regexp)
  "Judge if CMDS will generate REGEXP as output."
  `(with-current-buffer (apply 'term-run
                               (car ,cmds)
                               nil
                               (cdr ,cmds))
     (sleep-for 0.5)
     (goto-char (point-min))
     (search-forward-regexp ,regexp)))

(defmacro ert-term-run-shell-command-make-output (commandline regexp)
  "Judge if COMMANDLINE will generate REGEXP as output."
  `(with-current-buffer (term-run-shell-command ,commandline)
     (sleep-for 0.5)
     (goto-char (point-min))
     (search-forward-regexp ,regexp)))

(ert-deftest test-term-run ()
  (ert-term-run-make-output '("printf" "%04d" "112")
                            "^0112$"))

(ert-deftest test-term-run-shell-command ()
  (ert-term-run-shell-command-make-output "printf '%05d' 345"
                                          "^00345$"))
