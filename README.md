# CodeGra.el
The emacs plugin to interact with CodeGra.de through CodeGra.fs.

## Dependencies
For this plugin to work correctly you need to have
[CodeGra.fs](https://github.com/CodeGra-de/CodeGra.fs) installed. This means
that `cgfs` and `cgapi-consumer` need to be in your `$PATH`. Furthermore this
plugin depends on the
[switch-buffer-functions](https://github.com/10sr/switch-buffer-functions-el)
package, which can be installed using `MELPA`.

## Install
To install this package first install all its dependencies (see above). Now
clone this repository to some local folder and add this folder to your
`load-path` in emacs. After doing this you can add `(require 'codegrade)` to
your emacs config.

## Usage
There are two main functions in this package, editing rubrics and giving line
feedback. It is important to note that this package does not yet check if the
file-system if mounted in `--fixed` mode, please make sure it is when giving
line feedback.

### Editing rubric
To edit a rubric you call the `codegrade-open-rubric` function. This opens a
rubric in a new buffer with the major mode `codegrade-rubric-mode` that you can
edit. To toggle a rubric item you can call `codegrade-toggle-rubric-item` which
is bound to `c` and `,` by default in the `codegrade-rubric-mode`. You can goto
the next item with `codegrade-goto-next-item` (bound to <kbd>n</kbd>), to the
previous item with `codegrade-goto-previous-item` (bound to <kbd>p</kbd>). To
goto next or previous headers use the `codegrade-goto-*-header` functions (bound
to <kbd>N</kbd> and <kbd>P</kbd> by default). To quit this rubric you should use
the `codegrade-rubric-close` function, bound to <kbd>q</kbd>.

The opened buffer follows you around. So if you open another submission by
another user the rubric buffer automatically updates. See the first line of this
buffer to see which person you are grading.

## Line feedback
To edit line feedback you can call the `codegrade-add-feedback` function on a
line. This opens a new buffer in the `codegrade-feedback-mode` mode. You can
save the contents in this buffer and quit by calling `codegrade-feedback-close`
(bound to <kbd>C-c C-c</kbd> by default). To quit without saving you can call
`codegrade-feedback-quit`, bound to <kbd>C-c C-k</kbd> by default.

You can also delete a line of feedback by calling
`codegrade-delete-feedback`. To see the feedback of a line you can call
`codegrade-get-feedback`.

## Giving grades
You can also give a grade using this plugin. Simply call `codegrade-give-grade`
and input a number between 0 and 10.
