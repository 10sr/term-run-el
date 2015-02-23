[![Build Status](https://travis-ci.org/10sr/term-run-el.svg)](https://travis-ci.org/10sr/term-run-el)


term-run-el
===========

Run command in terminal buffer.


Usage
-----


* term-run-shell-command (command $optional new-buffer-p)

  Run COMMAND in terminal buffer.

  If NEW-BUFFER-P is given or called with prefix argument, generate new
terminal buffer for running COMMAND.  Otherwise, use the same buffer.  In
this case, old process in the buffer will be destroyed.

  This function is intended mainly to be called interactively.


* term-run (program &optional buffer-or-name &rest args)

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
