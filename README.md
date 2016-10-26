# mode on region

Please prefer the documentation in file **mode-on-region.el**.
This readme file may not be kept up to date.

**mor-mode-on-region** copies a highlighted region to a tmp buffer and
turns on the mode of your choice.

**mor-prev-mode-on-region** does the same thing, but recalls the previously
chosen mode.

Copy the text back with **mor-copy-back**

Abandon your edits with **mor-close-tmp-buffer**


This package is useful to work with code in mutli-lanuage files.  Such as
javascript, css, and html mixed in one file.

It's also useful to interact with code in technical books; while viewing via
eww-mode, text-mode, info-mode, etc.


# Installation

Place mode-on-region.el in folder /your/chosen/folder.

Then add the following text to your .emacs or init.el file:

    (add-to-list 'load-path "/your/chosen/folder")
    (autoload #'mor-mode-on-region "mode-on-region" nil t)
    (autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
    ;; configure
    (setq mor-format-automatically-p nil)
    (setq mor-readonly-for-extra-protection-p t)
    (custom-set-faces
     `(mor-readonly-face
       ((t (:background "black" :foreground "red" :strike-through t)))))
    ;; recommended keybinds for vanilla Emacs.  Press "C-c m" with text highlighted.
    (global-set-key (kbd "C-c m") #'mor-mode-on-region)
    (global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
    ;; recommended keybinds for evil users.  Press "m" in visual mode.
    (eval-after-load 'evil
      '(progn
         (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
         (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)))
    ;; recommended keybinds for the tmp buffer.  Both Vanilla and Evil.
    (eval-after-load 'mode-on-region
      '(progn
         (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
         (define-key mor-tmp-buffer-mode-map (kbd "C-c c") #'mor-close-tmp-buffer)))