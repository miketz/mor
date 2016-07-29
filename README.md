# mor - mode on region

**mor-mode-on-region** copies a highlighted region to a tmp buffer and
turns on the mode of your choice.
`mor-prev-mode-on-region' does the same thing, but recalls the previously
chosen mode.

Copy the text back with **mor-copy-back** or C-x C-c b.
                                             mnemonic: copy back
Abandon your edits with **mor-close-tmp-buffer** or C-x C-c c.
                                                    mnemonic: close

This package is useful to work with code in mutli-lanuage files.  Such as
javascript, css, and html mixed in one file.

It's also useful to interact with code in tecnical books; while viewing via
eww-mode, text-mode, info-mode, etc.
Some Emacs compatible books:
  -- The `sicp' package on melpa is a great info-mode book.
  -- The Practical Common Lisp in html form.  View with eww.
  -- The Emacs Lisp intro, bundle with Emacs by default.  info-mode.


NOTE: lexical binding is used as a potential micro-optimization for
variable lookups.  This package *should* work whether lexical or dynamic
binding is used.
