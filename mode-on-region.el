;;; mode-on-region.el --- Mode on region -*- lexical-binding: t -*-

;;; License: GPL version 3


;;; Commentary:
;;; `mor-mode-on-region' copies a highlighted region to a tmp buffer and
;;; turns on the mode of your choice.
;;; `mor-prev-mode-on-region' does the same thing, but recalls the previously
;;; chosen mode.
;;;
;;; Copy the text back with `mor-copy-back' or C-c b.
;;;                                            mnemonic: copy back
;;; Abandon your edits with `mor-close-tmp-buffer' or C-c c.
;;;                                                   mnemonic: close
;;;
;;; This package is useful to work with code in mutli-lanuage files.  Such as
;;; javascript, css, and html mixed in one file.
;;;
;;; It's also useful to interact with code in tech books; while viewing via
;;; eww-mode, text-mode, info-mode, etc.
;;; Some Emacs compatible books:
;;;   -- The `sicp' package on melpa is a great info-mode book.
;;;   -- The Practical Common Lisp in html form.  View with eww.
;;;   -- The Emacs Lisp intro, bundled with Emacs by default.  info-mode.
;;;
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable lookups.  This package *should* work whether lexical or dynamic
;;; binding is used.
;;; **This is no longer true.  Lexical binding is now used as way of providing
;;; private variables.  It is no longer just a micro-optimization.
;;;
;;; Dedication:  Doing it all for Leyna.


;;; Installation:
;;; Place `mode-on-region.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;;   (add-to-list 'load-path "/your/chosen/folder")
;;;   (autoload #'mor-mode-on-region "mode-on-region" nil t)
;;;   (autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
;;;   ;; configure
;;;   (setq mor-format-automatically-p nil)
;;;   (setq mor-readonly-for-extra-protection-p t)
;;;   (custom-set-faces
;;;    `(mor-readonly-face
;;;      ((t (:background "black" :foreground "red" :strike-through t)))))
;;;   ;; recommended keybinds for vanilla Emacs.  Press "C-c m" with text highlighted.
;;;   (global-set-key (kbd "C-c m") #'mor-mode-on-region)
;;;   (global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
;;;   ;; recommended keybinds for evil users.  Press "m" in visual mode.
;;;   (eval-after-load 'evil
;;;     '(progn
;;;        (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
;;;        (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)))
;;;   ;; recommended keybinds for the tmp buffer.  Both Vanilla and Evil.
;;;   (eval-after-load 'mode-on-region
;;;     '(progn
;;;        (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
;;;        (define-key mor-tmp-buffer-mode-map (kbd "C-c c") #'mor-close-tmp-buffer)))


;;; Code:
(require 'cl-lib)
(require 'rx)

(defgroup mode-on-region nil
  "Mode on region"
  :prefix "mor-"
  :group 'tools)

(define-minor-mode mor-tmp-buffer-mode
  "Minor mode to simulate buffer local keybindings for mor tmp buffers.
Before the this minor mode, tmp buffer funcs were bound globally and
required guards to verify the user was inside a mor tmp buffer.
NOTE: the guards still exist for needed protection.  The minor mode
keybinds just help avoid keybind pollution, and reduce the risk of
accidentally calling a function not relevant outside of a tmp buffer."
  :lighter " mor-tmp"
  :keymap (let ((map (make-sparse-keymap)))
            ;; No default bindings for now. User will choose them.
            map))

(defvar mor-format-automatically-p nil
  "When t automatically format the copied text via `indent-region'.")

(defvar mor-switch-buff-fn #'switch-to-buffer-other-window
  "Function used to switch to the tmp buffer (and back again).
Choices: `switch-to-buffer-other-window' or `switch-to-buffer'")

(defvar mor-readonly-for-extra-protection-p t
  "When t make the selected region of the orig buffer read-only.
Until the tmp buffer is killed.  This prevents edits in the orig buffer from
being accidentally overwritten.")

(defface mor-readonly-face
  '((t (:inherit region)))
  "Face for the selected region.
When using `mor-readonly-for-extra-protection-p'"
  :group 'mode-on-region)


;; TODO: Fix bug where tmp buffer won't die if the orig buffer is killed first.
;;       Problaby need some guards when attemping to dispose markers in the
;;       no-longer-existing orig buff.
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

(defvar-local mor--orig-buffer nil
  "The original buffer you highted some text in.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--start nil
  "Start of region. Implemented via a marker.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--end nil
  "End of region. Implemented via a marker.
Used in tmp buffer to transfer the modified text back to the original buffer.")

;; `mor--prefix' used for tmp buffer names. Make it private by let-binding it
;; and accessing it with lexical scope.
(let ((mor--prefix "mor-tmp-"))
  (defun mor-kill-tmp-buffers ()
    "Delete the junk tmp buffers."
    (interactive)
    (dolist (b (buffer-list))
      (when (mor--starts-with-p (buffer-name b) mor--prefix)
        (kill-buffer b))))

 ;; `seq' is a sequential counter used to generate unique names for tmp
 ;; buffers. Make it private by let-binding it and accessing it with lexical
 ;; scope.
 (let ((seq 0))
   (defun mor--gen-buffer-name ()
     "Generate a unique buffer name."
     (prog1
         (concat mor--prefix
                 (buffer-name (current-buffer))
                 "-"
                 (int-to-string seq))
       (cl-incf seq)))))

(defun mor--starts-with-p (string prefix)
  "Return t if STRING begins with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun mor--overlap-p (start1 end1 start2 end2)
  "Determines if 2 ranges overlap.
START1 = range 1 start.
END1 = range 1 end.
START2 = range 2 start.
END2 = range 2 end."
  (cl-labels ((between? (start end loc)
                        (and (>= loc start)
                             (<= loc end))))
    (or (between? start1 end1 start2)
        (between? start1 end1 end2))))

(defun mor--set-region (state start end)
  "Make region writeable or readonly based on STATE.
If STATE=readonly make region readonly.
If STATE=writeable make region writeable.
START of region.
END of region."
  ;; based on phils function:
  ;; http://stackoverflow.com/questions/20023363/emacs-remove-region-read-only
  (let ((modified (buffer-modified-p))
        ;; TODO: fully handle case when region starts at first pos in buffer.
        (start-adj (if (> start 1) (1- start) start)))
    ;; shadow `buffer-undo-list' with dynamic binding. We don't want the
    ;; read-only text property to be recorded in undo. Otherwise the user
    ;; may freeze a section of their buffer after an undo!!!!
    (let ((buffer-undo-list))
      (if (eq state 'readonly)
          (progn
            (overlay-put (make-overlay start end) 'face 'mor-readonly-face)
            (add-text-properties start-adj end '(read-only t)))
        ;; else make writeable
        (let ((inhibit-read-only t)) ;; Do i need this?
          (remove-text-properties start-adj end '(read-only t))
          (remove-overlays start end))))
    (set-buffer-modified-p modified)))

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
                                  (apropos-internal "-mode$"
                                                    #'commandp)
                                  nil t)))))

;; `mor--prev-mode-fn' is the the previous mode used. Make it private by
;; let-binding it and accessing it with lexical scope.
(let ((mor--prev-mode-fn nil))

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
        ;; GUARD: can't make it readonly if the buffer is already readonly.
        ;; TODO: improve guard to prevent 2 tmp buffers having overlapping
        ;;       regions. Currenlty there is a bug where the overlay will
        ;;       remain if 2 tmp buffers have overlapping regions.
        (unless buffer-read-only
          ;; lock down region in `orig-buff' until `tmp-buff' is killed
          (mor--set-region 'readonly start end)))

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
              ;; track start/end with markers. Markers will automatically "move"
              ;; as the text around them is changed.
              mor--start (set-marker (make-marker) start orig-buff)
              mor--end (set-marker (make-marker) end orig-buff)))

      (mor-tmp-buffer-mode) ; for keybinds.

      ;; show a header with useful keybind info. Like `org-src-mode' does.
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "[Copy back]: \\[mor-copy-back]  [Abort]: \\[mor-close-tmp-buffer]"))

      (when mor-format-automatically-p
        (indent-region (point-min) (point-max))))))

(defun mor-copy-back ()
  "Copy the tmp buffer text back the original buffer.

WARNING:
Overwrites the original text."
  (interactive)
  (if (null mor--orig-buffer) ; guard
      (message "You must be in a mor-tmp buffer for this to work.")
    ;; else
    ;; Cache tmp buffer local values. They will be invisible once we switch
    ;; back to the orig buffer.
    (let* ((tmp-buff (current-buffer))
           (start (marker-position mor--start))
           (rng (- (marker-position mor--end)
                   start)))

      (mor--unlock-orig-buffer)

      ;; copy tmp buffer text.
      (kill-ring-save (point-min) (point-max))

      ;; Switch to orig buff. Buffer local values will be invisible!
      (funcall mor-switch-buff-fn mor--orig-buffer)

      ;; delete original selected region
      (goto-char start)
      (delete-char rng)
      ;; paste new text
      (yank)

      ;; kill the tmp buffer becuase mulitple attempts to copy back text
      ;; will be wrong due to the now invalid start/end location. Will need
      ;; to use a better way to track start/end before we can allow the
      ;; tmp buffer to live longer for mulitple copies.
      (quit-window t (get-buffer-window tmp-buff)))))

(defun mor-close-tmp-buffer ()
  "Kill the tmp buffer and clean up the window if applicable.
Call this if you don't want to copy the text back to the original buffer."
  (interactive)
  (quit-window t))


(defun mor--marker-active-p (m)
  "Return t if the marker is actively pointing to a position.
M for marker."
  (and (not (null m))
       (not (null (marker-position m)))))

(defun mor--unlock-orig-buffer ()
  "Unlock region in the original buffer."

  (when mor-readonly-for-extra-protection-p
    ;; guard against dupe call from hook
    (when (and (mor--marker-active-p mor--start)
               (mor--marker-active-p mor--end))
      (let ((start (marker-position mor--start))
            (end (marker-position mor--end)))
        (with-current-buffer mor--orig-buffer
          (mor--set-region 'writeable start end)))))

  ;; clear markers
  (when (mor--marker-active-p mor--start) ; guard against dupe call from hook
    (set-marker mor--start nil))
  (when (mor--marker-active-p mor--start) ; guard against dupe call from hook
    (set-marker mor--end nil)))


;; use a hook to unlock the orig buffer when the tmp buffer is killed
(add-hook 'mor-tmp-buffer-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook
                      'mor--unlock-orig-buffer
                      nil
                      'make-it-local)))


(provide 'mode-on-region)

;;; Unit tests.
(when nil ;; Do not execute.
  ;; TODO: use ert. For now it's just a progn I manually eval.
  (progn
    (require 'mode-on-region)
    ;; mor--overlap-p
    (cl-assert (not (mor--overlap-p 1 10 15 20)))
    (cl-assert (not (mor--overlap-p 2 10 -1 1)))
    (cl-assert (mor--overlap-p 2 10 3 4))
    (cl-assert (mor--overlap-p 2 10 3 40))
    (cl-assert (mor--overlap-p 2 10 -3 5))
    'pass))

;;; mode-on-region.el ends here
