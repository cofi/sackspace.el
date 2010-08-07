Install
=======

* Put `sackspace.el` into your `load-path` (`make compile` will give you a byte-compiled version)
* Load file and run `sack/install-in-viper` or `sack/install-in-emacs`.

Usage
=====

You can use `M-x customize-group sackspace` to change the keybindings of
sack/install-*. 

Keybindings
-----------

Backspace       : Delete char backwards or delete back Tabstop [1]

C-Backspace     : Delete word backwards (you can customize delete function) [1]

Alt-Backspace   : Delete char backwards [1]

Shift-Backspace : Delete all whitespace till non-whitespace char is found.

[1] Takes a numeric argument. You can specify how often it will be executed.

Issues with other Packages
==========================

Direct: None.

Indirect:

* Viper

`sack/install-in-viper` will take care that `viper-vi-style-in-minibuffer` is
nil, but you can circumvent that behavior via `sack/force-viper-install`. But
note that if you do so it is very likely that you face unwanted behavior (like
facading the bindings in Ido).
