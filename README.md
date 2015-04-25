jscs-indent.el
==============

jscs-indent applies JSCS indentation rules to JavaScript modes.

Installation
------------

1. Install [JSCS](http://jscs.info/).

2. Put `jscs-indent.el` somewhere in your `load-path`.

3. Add the following code into your .emacs:

    ```el
    (with-eval-after-load 'js
      (require 'jscs-indent)
      (add-hook 'js-mode-hook #'jscs-indent-apply))

    (with-eval-after-load 'js2-mode
      (require 'jscs-indent)
      (add-hook 'js2-mode-hook #'jscs-indent-apply))
    ```
