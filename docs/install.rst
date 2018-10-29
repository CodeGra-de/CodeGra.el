Installation
=============
The CodeGrade filesystem has to be installed and both the ``cgfs`` and the
``cgapi-consumer`` helper program must be available from the user's ``$PATH`` to
successfully install and use the CodeGrade editor plugins.

After the Filesystem is successfully installed clone
`this <https://github.com/CodeGra-de/CodeGra.el>`__ repository to a
local folder (using the ``git clone git@github.com:CodeGra-de/CodeGra.el.git``
commmand) and add  this folder to your ``load-path`` in emacs.

After doing this you can add ``(require 'codegrade)`` to your emacs config.

.. note::

    In addition to the general dependencies for all plugins, the Emacs plugin
    depends on the `switch-buffer-functions
    <https://github.com/10sr/switch-buffer-functions-el>`__ package, which can
    be installed using ``MELPA``.
