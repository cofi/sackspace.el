Install
=======

Put `sackspace.el` into your `load-path`.
(`make compile` will give you a byte-compiled version)

Usage
=====

Just load file and bind `sack/backspace` to a key (prefably backspace ;)
`sack/hyper-sack` is available for keybinding as well.

    (require 'sackspace)
    (global-set-key (kbd "<backspace>") 'sack/backspace)

Issues with other Packages
==========================

Direct: None.

Indirect:

* Viper

If you bind Sackspace in Insert-Mode like

    (define-key viper-insert-global-user-map (kbd "<backspace>") 'sack/backspace)
    (define-key viper-insert-global-user-map (kbd "C-<backspace>") 'sack/hyper-sack))

you will encounter Problems if `viper-vi-style-in-minibuffer' is non-nil,
because those facade bindings (like those of Ido) in the minibuffer.

Put

    (setq viper-vi-style-in-minibuffer nil)

in your ~/.viper to prevent that behaviour.
