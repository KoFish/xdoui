 XDO UI
========

A ncurses based UI for calling libxdo (from xdotool) functions to simulate
keypresses and mouse actions.

 Dependencies
--------------

 - xdotool

   The latest git version of xdotools is probably needed and it's available at
   https://github.com/jordansissel/xdotool . This is required by guile-xdo for
   libxdo.

 - guile-xdo

   A wrapper library for libxdo in GNU Guile that is available at
   https://github.com/KoFish/guile-xdo it should be pretty complete and
   hopefully useful for other things too.

 - guile-ncurses

   A wrapper library for ncurses in GNU Guile that is required for doing the
   whole UI stuff in XDO UI. It's available at
   http://www.gnu.org/software/guile-ncurses/


 The concepts
--------------

The interface is slightly inspired by vim but it's less intelligent. When you
start the application you get a parse-tree accepting keys by object to act on;
`w` is for windows, `m` is for mouse, `h` is for help and `q` is for quit, etc.
There are also a bunch of "helper" functions, such as `"` and `'` which changes
registers to use (more on that in a bit). There is also the concept of prefixes.

Prefix
++++++

A prefix is data that can be used in different functions besides the values
stored in registers. Prefixes always has to be entered manually and there are a
couple of ways to do that. Number prefixes can be entered by simply pressing
number keys before any command, string prefixes can be entered by pressing
enter, entering the text in the dialog that pops up, terminating with enter or
ctrl-d, and then doing the command.

Registers
+++++++++

When running XDO UI you have access to a whole bunch of registers, easiest of
them to use is probably a-z. When doing a command you can specify two different
registers, one load register and one store register. Store register is specified
by `"` followed by one of the accepted registers, and in that case the value
from the function called is stored in this register instead of being printed to
the main-window. Similary the `'` followed by a register changes the load
register, this register can be used to input values to a function.

Example: To move the currently active window to where the mouse curser is you
would do the following command `"agmp"bwga'awp` which can be read as following:
`g`et the current `m`ouse `p`osition into register `a`, get the active window
into register b, put window at position stored in register a. To but the window
at the position 100x200 you could do it by calling pe100x200wp, this you could
read as: `e`nter `p`osition 100x200, `p`ut `w`indow.

