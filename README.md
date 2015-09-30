jscs.el
=======

jscs.el provides consistent JavaScript editing using JSCS.

Installation
------------

1. Install [JSCS](http://jscs.info/).

2. Put `jscs.el` somewhere in your `load-path`.

3. Add the following code into your .emacs:

    ```el
    (autoload 'jscs-indent-apply "jscs" nil t)
    (autoload 'jscs-fix "jscs" nil t)
    (autoload 'jscs-fix-run-before-save "jscs" nil t)
    ```

Usage
-----

To apply JSCS indentation rules to JavaScript modes, add the following code into your .emacs:

```el
(with-eval-after-load 'js
  (add-hook 'js-mode-hook #'jscs-indent-apply))

(with-eval-after-load 'js2-mode
  (add-hook 'js2-mode-hook #'jscs-indent-apply))
```

To run `jscs --fix` interactively, run `M-x jscs-fix`.

To run `jscs --fix` on the current buffer when saving, add the following code into your .emacs:

```el
(add-hook 'js-mode-hook #'jscs-fix-run-before-save)
(add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
(add-hook 'js3-mode-hook #'jscs-fix-run-before-save)
```
