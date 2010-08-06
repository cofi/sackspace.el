Install
=======

* Put `sackspace.el` into your `load-path` (`make compile` will give you a byte-compiled version)
* Load file and run `sack/install-in-viper` or `sack/install-in-emacs`.

Usage
=====

Coming soon.

Issues with other Packages
==========================

Direct: None.

Indirect:

* Viper

`sack/install-in-viper` will take care that `viper-vi-style-in-minibuffer` is
nil, but you can circumvent that behavior via `sack/force-viper-install`, if you
do so it is very likely that you face unwanted behavior (like facading the
bindings in Ido).
