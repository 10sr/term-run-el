[![Build Status](https://travis-ci.org/10sr/term-run-el.svg)](https://travis-ci.org/10sr/term-run-el)
[![MELPA Stable](http://stable.melpa.org/packages/term-run-badge.svg)](http://stable.melpa.org/#/term-run)
[![MELPA](http://melpa.org/packages/term-run-badge.svg)](http://melpa.org/#/term-run)



term-run.el
===========

Run arbitrary command in a terminal buffer.

Currently Emacs provide `M-x term`, which uses `term-mode`
and works as a terminal-emulator.
It is useful to run shell programs like `bash`, `zsh` or `ipython`
interactively.
However, this function does not provide the feature to invoke programs *with
arguments*.

Here comes `term-run`, which provides functions to invoke arbitrary commands in
terminal buffers.
For example, you can run `ssh` command with any arguments in terminal buffer
directly (without running bash with `M-x term` first).
Try typing `M-x term-run-shell-command RET ssh git@github.com RET` !





Usage
-----


* `term-run-shell-command (command &optional new-buffer-p)`

  Run COMMAND in a terminal buffer.

  This function is intended mainly to be called interactively and
asks the command-line to invoke.

  If called with prefix argument, this function will generate new
terminal buffer for running COMMAND.  Otherwise, always use the buffer named
`*Term-Run Shell Command*`. In this case, the old process in the buffer will be
destroyed.



* `term-run (program &optional buffer-or-name &rest args)`

  Run PROGRAM in BUFFER-OR-NAME with ARGS in terminal buffer.

  If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in
the buffer will be destroyed.  Otherwise, new buffer will be generated
automatically from PROGRAM.

  This function returns the buffer where the process starts running.




License
-------


This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

See `LICENSE` for details.
