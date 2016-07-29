;;; mor.el --- Mode on region -*- lexical-binding: t -*-

;;; License: GPL version 3


;;; Commentary:
;;; `mor-mode-on-region' copies a highlighted region to a tmp buffer and
;;; turns on the mode of your choice.
;;; `mor-prev-mode-on-region' does the same thing, but recalls the previously
;;; chosen mode.
;;;
;;; Copy the text back with `mor-copy-back' or C-x C-c b.
;;;                                            mnemonic: copy back
;;; Abandon your edits with `mor-close-tmp-buffer' or C-x C-c c.
;;;                                                   mnemonic: close
;;;
;;; This package is useful to work with code in mutli-lanuage files.  Such as
;;; javascript, css, and html mixed in one file.
;;;
;;; It's also useful to interact with code in tecnical books; while viewing via
;;; eww-mode, text-mode, info-mode, etc.
;;; Some Emacs compatible books:
;;;   -- The `sicp' package on melpa is a great info-mode book.
;;;   -- The Practical Common Lisp in html form.  View with eww.
;;;   -- The Emacs Lisp intro, bundle with Emacs by default.  info-mode.
;;;
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable lookups.  This package *should* work whether lexical or dynamic
;;; binding is used.


;;; Installation:
;;; Place `mor.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;;   (add-to-list 'load-path "/your/chosen/folder")
;;;   (autoload #'mor-mode-on-region "mor" nil t)
;;;   (autoload #'mor-prev-mode-on-region "mor" nil t)
;;;   ;; configure
;;;   (setq mor-format-automatically-p nil)
;;;   (setq mor-readonly-for-extra-protection-p t)
;;;   ;; recommended keybinds for vanilla Emacs.  Press "C-c m" with text highlighted.
;;;   (global-set-key (kbd "C-c m") #'mor-mode-on-region)
;;;   (global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
;;;   ;; recommended keybinds for evil users.  Press "m" in visual mode.
;;;   (eval-after-load "evil"
;;;     '(progn
;;;        (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
;;;        (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)))


;;; Code:
(require 'cl-lib)
(require 'rx)

(define-minor-mode mor-tmp-buffer-mode
  "Minor mode to simulate buffer local keybindings for mor tmp buffers.
Before the this minor mode, tmp buffer funcs were bound globally and
required guards to verify the user was inside a mor tmp buffer.
NOTE: the guards still exist for needed protection.  The minor mode
keybinds just help avoid keybind pollution, and reduce the risk of
accidentally calling a function not relevant outside of a tmp buffer."
  :lighter " mor-tmp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-c b") #'mor-copy-back)
            (define-key map (kbd "C-x C-c C-b") #'mor-copy-back)
            (define-key map (kbd "C-x C-c c") #'mor-close-tmp-buffer)
            (define-key map (kbd "C-x C-c C-c") #'mor-close-tmp-buffer)
            map))

(defvar mor-readonly-for-extra-protection-p t
  "When t the orig buffer will be read only until the tmp buffer is killed.
This prevents edits in the orig buffer from throwing off the static coordinates
for copying text back.
NOTE: This is not full-proof.")

(defvar mor-format-automatically-p nil
  "When t automatically format the copied text via `indent-region'.")

(defvar mor-switch-buff-fn #'switch-to-buffer-other-window
  "Function used to switch to the tmp buffer (and back again).
Choices: `switch-to-buffer-other-window' or `switch-to-buffer'")


;; TODO: Make an option to attempt to preserve the original indent when copying
;;       text back to the original buffer. This could make option
;;       `mor-format-automatically-p' more useful becuase when it forces
;;       text to the left edge in tmp, it won't destroy the indent in the orig
;;       buffer.
;; TODO: Support selection of rectangular regions. Useful for selecting text
;;       in a comment. So you could exlude the comment markers that would mess
;;       up the dedicated mode buffer.
;; TODO: Incorporate code from org-mode. Locks highlighted region from edits.
;;       Look into how it copies text back/forth between buffers.
;;       See function `org-edit-special'
;; TODO: Optionally create a tmp file on disk. Useful for features that
;;       require a file on disk (some linters, etc).

(defconst mor--prefix "mor-tmp-"
  "Prefix used for tmp buffer names.")
(defvar mor--counter 0
  "Sequential counter used to generate unique names for tmp buffers.")

(defvar mor--prev-mode-fn nil
  "The previous mode used.
Used by `mor-prev-mode-on-region'")


(defvar-local mor--orig-buffer nil
  "The original buffer you highted some text in.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--start nil
  "Start of region.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--end nil
  "End of region.
Used in tmp buffer to transfer the modified text back to the original buffer.")


(defun mor--gen-buffer-name ()
  "Generate a unique buffer name."
  (prog1
      (concat mor--prefix
              (buffer-name (current-buffer))
              "-"
              (int-to-string mor--counter))
    (cl-incf mor--counter)))

(defun mor--starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun mor-kill-tmp-buffers ()
  "Delete the junk tmp buffers."
  (interactive)
  (dolist (b (buffer-list))
    (when (mor--starts-with-p (buffer-name b) mor--prefix)
      (kill-buffer b))))

(defvar mor-mode-fn nil
  "Making mode-fn a dynamic variable.
So I can take advantage of dynamic binding.
Keep this value nil.  Only bind it dynamically in a `let' statement.")

;;;###autoload
(defun mor-mode-on-region (start end)
  "Switch to a new buffer with the highlighted text.
Turn on the selected mode.
Region is between START and END inclusive."
  (interactive "r")
  (mor--mode-on-region start
                       end
                       (if mor-mode-fn
                           ;; use dynamcally bound value if available.
                           mor-mode-fn
                         ;; otherwise user chooses the mode
                         (intern (completing-read
                                  "Mode: "
                                  (mapcar (lambda (e)
                                            (list (symbol-name e)))
                                          (apropos-internal "-mode$"
                                                            #'commandp))
                                  nil t)))))

;;;###autoload
(defun mor-prev-mode-on-region (start end)
  "Same as `mor-mode-on-region' but use the previous mode.
Previous mode is saved in variable `mor--prev-mode-fn'.
Region is between START and END inclusive."
  (interactive "r")
  (if (null mor--prev-mode-fn) ; guard against early usage.
      (message "No previously used mode found.")
    (mor--mode-on-region start
                         end
                         mor--prev-mode-fn)))

(defun mor--mode-on-region (start end mode-fn)
  "The core function to copy region to a new buffer.
Region is between START and END.
MODE-FN the function to turn on the desired mode."

  ;; remember the mode for `mor-prev-mode-on-region'
  (setq mor--prev-mode-fn mode-fn)

  ;; save buffer and region coordinates to copy the text back in later.
  (let ((orig-buff (current-buffer))
        (tmp-buff (mor--gen-buffer-name)))


    (kill-ring-save start end) ;; copy higlighted text
    (when mor-readonly-for-extra-protection-p
      ;; lock down `orig-buff' until `tmp-buff' is killed
      (read-only-mode 1))
    (deactivate-mark)

    (funcall mor-switch-buff-fn tmp-buff)
    (yank)              ;; paste text
    ;; ignore errors before turning on mode, otherwise mor keybinds won't be
    ;; set. Like "C-c c" to close.
    (with-demoted-errors "Error: %S"
      (funcall mode-fn)) ;; turn on the dedicated mode.
    (with-current-buffer tmp-buff
      ;; NOTE: these buffer-local vars must be set AFTER `mode-fn' is
      ;; called. Because major modes wipe buffer local vars.
      (setq mor--orig-buffer orig-buff
            mor--start start
            mor--end end))

    (mor-tmp-buffer-mode) ; for keybinds.

    ;; show a header with useful keybind info. Like `org-src-mode' does.
    (set (make-local-variable 'header-line-format)
         (substitute-command-keys
          "[Copy back]: \\[mor-copy-back]  [Abort]: \\[mor-close-tmp-buffer]"))

    (when mor-format-automatically-p
      (mark-whole-buffer)
      ;; using `call-interactively' because it includes the START/END
      ;; region parameters.
      (call-interactively #'indent-region))))

(defun mor-copy-back ()
  "Copy the tmp buffer text back the original buffer.

WARNING:
Overwrites the original text.
May not work correclty if original buffer has been modified since the tmp
buffer was created.  If in doubt, just manually copy the text back.

TODO: Use a more full-proof technqiue to identify the start/end location to
overwrite."
  (interactive)
  (mor--finished-with-tmp-buffer t))

(defun mor-close-tmp-buffer ()
  "Kill the tmp buffer and clean up the window if applicable.
Call this if you don't want to copy the text back to the original buffer."
  (interactive)
  (quit-window t))

(defun mor--unlock-orig-buffer ()
  "Unlock the original buffer."
  (when mor-readonly-for-extra-protection-p
    (with-current-buffer mor--orig-buffer
      (read-only-mode 0))))
;; use a hook to unlock the orig buffer when the tmp buffer is killed
(add-hook 'mor-tmp-buffer-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook
                      'mor--unlock-orig-buffer
                      nil
                      'make-it-local)))

(defun mor--finished-with-tmp-buffer (copy-back-p)
  "Manages the end of life of the tmp buffer.

When COPY-BACK-P is t, copy text back to the original buffer.

Kills the tmp buffer."
  (if (null mor--orig-buffer) ; guard
      (message "You must be in a mor-tmp buffer for this to work.")
    (progn ; else

      (mor--unlock-orig-buffer)

      ;; Cache tmp buffer local values. They will be invisible once we switch
      ;; back to the orig buffer.
      (let ((tmp-buff (current-buffer))
            (start mor--start)
            (rng (- mor--end
                    mor--start)))
        (when copy-back-p
          ;; tmp buffer text.
          (kill-ring-save (point-min) (point-max)))

        ;; Switch to orig buff. Buffer local values will be invisible!
        (funcall mor-switch-buff-fn mor--orig-buffer)

        (when copy-back-p
          ;; delete original selected region
          (goto-char start)
          (delete-char rng)
          ;; paste new text
          (yank))

        ;; kill the tmp buffer becuase mulitple attempts to copy back text
        ;; will be wrong due to the static start/end location. Will need
        ;; to use a better way to track start/end before we can allow the
        ;; tmp buffer to live longer for mulitple copies.
        (quit-window t (get-buffer-window tmp-buff))))))


(provide 'mor)

;;; mor.el ends here
