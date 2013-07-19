Install
=======

* Put `sackspace.el` into your `load-path` (`make compile` will give you a byte-compiled version). Alternatively install it via `package.el` from [Marmalade](http://marmalade-repo.org/)
* Use `(sackspace-mode 1)` to enable it

Usage
=====


Keybindings
-----------

Backspace       : Delete char backwards or delete back Tabstop (or an equivalent number of spaces) [1]

C-Backspace     : Delete word backwards (you can customize delete function) [1]

Alt-Backspace   : Delete char backwards [1]

Shift-Backspace : Delete all whitespace till non-whitespace char is found.

[1] Takes a numeric argument. You can specify how often it will be executed.

Note: If you use a terminal Emacs C-Backspace and Shift-Backspace are not available.

Other Packages
--------------

Supports `subword-mode`, and `paredit-mode`.
To disable this support change the "honor" customs in `M-x customize-group RET sackspace RET`.

Supports `evil` directly by prohibiting edits in non-editing states if evil is enabled.

Limitations
===========

* Within `term-mode` `sacks/whitespace` won't work (it will delete the chars from the emacs buffer, but not from the terminal).
