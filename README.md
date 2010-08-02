<!-- -*- mode: markdown -*- -->
Install
-------

Put `sackspace.el` into your `load-path`.
(`make compile` will give you a byte-compiled version)

Usage
-----

Just load file and bind `sack/backspace` to a key (prefably backspace ;)
`sack/hyper-sack` is available for keybinding as well.

    (require 'sackspace)
    (global-set-key (kbd "<backspace>") 'sack/backspace)

Issues with other Packages
==========================

Direct: None.
Indirect:
 * Viper+Ido
   If you bind Sackspace in Insert-Mode like

    (define-key viper-insert-global-user-map (kbd "<backspace>") 'sack/backspace)
    (define-key viper-insert-global-user-map (kbd "C-<backspace>") 'sack/hyper-sack))

   you will encounter Problems with Ido, because those bindings facade somehow
   those in `'ido-file-dir-completion-map`.
   **Workaround**:
   Bind a new key for `'ido-up-directory` like following for Up-Arrow
   
    (add-hook 'ido-setup-hook
       (lambda ()
         (define-key ido-file-dir-completion-map (kbd "<up>")
                                                 'ido-up-directory)))
