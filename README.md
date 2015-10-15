jscs.el
=======

jscs.el helps consistent JavaScript editing using JSCS.

Features
--------

- Apply indentation rules from JSCS config file
- Run `jscs --fix`
- Support all types of JSCS config files

Requirements
------------

- Emacs 24 or later
- [JSCS][] 2.0.0 or later

Make sure that `node` and `jscs` commands can be executed from Emacs: `M-! node --version` and `M-! jscs --version`.

Installation
------------

jscs.el is available on [MELPA][].

Usage
-----

To apply JSCS indentation rules to JavaScript or JSON modes, add the following code into your .emacs:

```el
(add-hook 'js-mode-hook #'jscs-indent-apply)
(add-hook 'js2-mode-hook #'jscs-indent-apply)
(add-hook 'json-mode-hook #'jscs-indent-apply)
```

To run `jscs --fix` interactively, run `M-x jscs-fix`.

To run `jscs --fix` on JavaScript modes when saving, add the following code into your .emacs:

```el
(add-hook 'js-mode-hook #'jscs-fix-run-before-save)
(add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
```

Indentation Rules
-----------------

- [validateIndentation][]
  - `indent-tabs-mode`
  - `js-indent-level` in js-mode or json-mode
  - `js2-basic-offset` in js2-mode
- [maximumLineLength][]
  - `tab-width`

[JSCS]: http://jscs.info/
[MELPA]: http://melpa.org/
[validateIndentation]: http://jscs.info/rule/validateIndentation
[maximumLineLength]: http://jscs.info/rule/maximumLineLength
