(require 'term-run)

(defmacro ert-term-run-make-output (cmd str)
  "Judge if CMD will generate STR as output."
  `(with-current-buffer (apply 'term-run
                               (car ,cmd)
                               nil
                               (cdr ,cmd))
     (sleep-for 0.5)
     (goto-char (point-min))
     (search-forward ,str)))

(defmacro ert-term-run-shell-command-make-output (cmd str)
  "Judge if CMD will generate STR as output."
  `(with-current-buffer (term-run-shell-command ,cmd)
     (sleep-for 0.5)
     (goto-char (point-min))
     (search-forward ,str)))

(ert-deftest test-term-run ()
  (ert-term-run-make-output '("printf" "%04d" "112")
                            "0112"))

(ert-deftest test-term-run-shell-command ()
  (ert-term-run-shell-command-make-output "printf '%05d' 345"
                                          "00345"))
