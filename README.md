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

    (with-eval-after-load 'js
      (add-hook 'js-mode-hook #'jscs-indent-apply))

    (with-eval-after-load 'js2-mode
      (add-hook 'js2-mode-hook #'jscs-indent-apply))
    ```
