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
;;; This package is useful to work with code in multi-language files.  Such as
;;; JavaScript, CSS, and HTML mixed in one file.
;;;
;;; It's also useful to interact with code in tech books; while viewing via
;;; eww-mode, text-mode, info-mode, etc.
;;; Some Emacs compatible books:
;;;   -- The `sicp' package on melpa is a great info-mode book.
;;;   -- The Practical Common Lisp in HTML form.  View with eww.
;;;   -- The Emacs Lisp intro, bundled with Emacs by default.  info-mode.
;;;
;;;
;;; NOTE: lexical binding is used as a potential micro-optimization for
;;; variable look-ups.  This package *should* work whether lexical or dynamic
;;; binding is used.
;;; **This is no longer true.  Lexical binding is now used as way of providing
;;; private variables.  It is no longer just a micro-optimization.
;;;
;;; Dedication:  Doing it all for Leyna.


;;; Installation:
;;; Place `mode-on-region.el' in folder `/your/chosen/folder'.
;;; Add the following text to your .emacs or init.el file:
;;;
;;; (add-to-list 'load-path "/your/chosen/folder")
;;; (autoload #'mor-mode-on-region "mode-on-region" nil t)
;;; (autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
;;; ;; Recommended keybinds for vanilla Emacs.  Press "C-c m" with text highlighted.
;;; (global-set-key (kbd "C-c m") #'mor-mode-on-region)
;;; (global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
;;; ;; Recommended key binds for evil users.  Press "m" in visual mode.
;;; (eval-after-load 'evil
;;;   '(progn
;;;      (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
;;;      (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)))
;;; ;; Configure
;;; (eval-after-load 'mode-on-region
;;;   '(progn
;;;      (setq mor-format-automatically-p nil)
;;;      (setq mor-fix-whitespace-p nil)
;;;      (setq mor-readonly-for-extra-protection-p t)
;;;      (custom-set-faces
;;;       `(mor-readonly-face
;;;         ((t (:background "black" :foreground "red" :strike-through t)))))
;;;      ;; recommended key binds for the tmp buffer.  Both Vanilla and Evil.
;;;      (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
;;;      (define-key mor-tmp-buffer-mode-map (kbd "C-c c") #'mor-close-tmp-buffer)))


;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'whitespace)

(defconst mor--minimal-emacs "24.1"
  "Minimum Emacs version needed to run mode-on-region.
This version introduced lexical binding.")

(defun mor-assert-dependencies ()
  "Check for required dependencies.  Warn the user if any are missing."
  (when (version< emacs-version
                  mor--minimal-emacs)
    (display-warning
     'mode-on-region
     (format "mode-on-region requires Emacs >= %s, you are using %s."
             mor--minimal-emacs emacs-version)
     :error)))

;; Invoke dependency check at load time of mode-on-region.
;; Only one check.  Don't prevent use of the feature.  Just warn then let the
;; chips fall where they may.
(mor-assert-dependencies)

(defgroup mode-on-region nil
  "Mode on region"
  :prefix "mor-"
  :group 'tools)

(define-minor-mode mor-tmp-buffer-mode
  "Minor mode to simulate buffer local keybinds in mor tmp buffers.
Before this minor mode, tmp buffer funcs were bound globally and
required guards to verify the user was inside a mor tmp buffer.
NOTE: the guards still exist for needed protection.  The minor mode
keybinds just help avoid keybind pollution, and reduce the risk of
accidentally calling a function not relevant outside of a tmp buffer."
  :lighter " mor-tmp"
  :keymap (let ((map (make-sparse-keymap)))
            ;; No default bindings for now. User will choose them.
            map))

(defcustom mor-format-automatically-p nil
  "When t automatically format the copied text via `indent-region'."
  :type 'boolean
  :group 'mode-on-region)

(defcustom mor-fix-whitespace-p nil
  "When t automatically fix whitepsace via `whitespace-cleanup'."
  :type 'boolean
  :group 'mode-on-region)

(defcustom mor-switch-buff-fn #'switch-to-buffer-other-window
  "Function used to switch to the tmp buffer (and back again).
Choices: `switch-to-buffer-other-window' or `switch-to-buffer'"
  :type 'function
  :group 'mode-on-region)

(defcustom mor-readonly-for-extra-protection-p t
  "When t make the selected region of the orig buffer read-only.
Until the tmp buffer is killed.  This prevents edits in the orig buffer from
being accidentally overwritten."
  :type 'boolean
  :group 'mode-on-region)

(defface mor-readonly-face
  '((t (:inherit region)))
  "Face for the selected region.
When using `mor-readonly-for-extra-protection-p'"
  :group 'mode-on-region)

;; Closure functions (created in a let) seem to be undetected by the byte
;; compiler or flycheck.  Suppress warnings by redundantly declaring the
;; functions here.
(declare-function mor-get-tmp-buffers 'mode-on-region)
(declare-function mor--gen-buffer-name 'mode-on-region)
(declare-function mor--mode-on-region 'mode-on-region)

;; TODO: Fix bug where tmp buffer won't die if the orig buffer is killed first.
;;       Probably need some guards when attempting to dispose markers in the
;;       no-longer-existing orig buff.
;; TODO: Make an option to attempt to preserve the original indent when copying
;;       text back to the original buffer. This could make option
;;       `mor-format-automatically-p' more useful because when it forces
;;       text to the left edge in tmp, it won't destroy the indent in the orig
;;       buffer.
;; TODO: Support selection of rectangular regions. Useful for selecting text
;;       in a comment. So you could exclude the comment markers that would mess
;;       up the dedicated mode buffer.
;; TODO: Incorporate code from org-mode. Locks highlighted region from edits.
;;       Look into how it copies text back/forth between buffers.
;;       See function `org-edit-special'
;; TODO: Optionally create a tmp file on disk. Useful for features that
;;       require a file on disk (some linters, etc).

;; Local to the tmp-buff
(defvar-local mor--orig-buffer nil
  "The original buffer you highted some text in.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--start nil
  "Start of region. Implemented via a marker.
Used in tmp buffer to transfer the modified text back to the original buffer.")
(defvar-local mor--end nil
  "End of region. Implemented via a marker.
Used in tmp buffer to transfer the modified text back to the original buffer.")


;; Local to the orig-buff
(defvar-local mor--overlays '()
  "Association list of overlays in the orig-buffer.
Ties each overlay to the relevant tmp buffer, so the overlay can be deleted
when the tmp-buffer is deleted.
Conceptually it would look something like this:
    ((tmp-buff1 . ov1)
     (tmp-buff2 . ov2))")

(defun mor--add-overlay-readonly (orig-buff tmp-buff start end)
  "Add readonly overlay in ORIG-BUFF and associate overlay with TMP-BUFF.
START of overlay region.
END of overlay region."
  ;; ensure buffer local var mor--overlays is set for orig-buff
  (with-current-buffer orig-buff
    (let ((ov (make-overlay start end orig-buff)))
      (overlay-put ov 'face 'mor-readonly-face)
      ;; associate ov with tmp-buff. So we can later delete ov when tmp-buff is
      ;; deleted.
      (push `(,tmp-buff . ,ov) mor--overlays)
      ;; return ov for convenience.
      ov)))

(defun mor--delete-overlay-readonly (orig-buff tmp-buff)
  "Delete the readonly overlay in ORIG-BUFF associated with TMP-BUFF."
  ;; ensure buffer local var mor--overlays is set for orig-buff
  (with-current-buffer orig-buff
    (let* ((entry (assoc tmp-buff mor--overlays))
           (ov (cdr entry)))
      (unless (null ov)
        (delete-overlay ov))
      ;; remove entry from assoc list
      (setq mor--overlays (remove entry mor--overlays)))))

;; `mor--prefix' used for tmp buffer names. Make it private by let-binding it
;; and accessing it with lexical scope.
(let ((mor--prefix "mor-tmp-"))
  (defun mor-get-tmp-buffers ()
    "Return a list of the mor tmp buffers."
    (let ((lst '()))
      (dolist (b (buffer-list))
        (when (mor--starts-with-p (buffer-name b) mor--prefix)
          (push b lst)))
      lst))

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

(defun mor-kill-tmp-buffers ()
  "Delete the junk tmp buffers."
  (interactive)
  (dolist (b (mor-get-tmp-buffers))
    (kill-buffer b))) ; NOTE: overlays are deleted automatically when the
                      ;       tmp-buffer is killed.

;; TODO: clean up orphaned "read-only" sections too, not just the overlay.
(defun mor-kill-overlays ()
  "Delete all overlays in the orig buffer.
You must be in the orig-buffer when you call this.  This is mostly just to clean
up any any orphaned overlays.  In theory this should never happen, but in a
bug-case where the tmp-buffer creation failed, we can't rely on the tmp-buffer
deletion hook to remove the overlay."
  (interactive)
  (dolist (entry mor--overlays)
    (let ((ov (cdr entry)))
      (delete-overlay ov)
      (setq mor--overlays (remove entry mor--overlays)))))

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
  (cl-labels ((between? (rng-start rng-end point)
                        (and (>= point rng-start)
                             (<= point rng-end))))
    (or (between? start1 end1 start2)
        (between? start1 end1 end2)
        (between? start2 end2 start1)
        (between? start2 end2 end1))))

(defun mor--set-region (state start end orig-buff tmp-buff)
  "Make region writable or readonly based on STATE.
If STATE=readonly make region readonly.
If STATE=writable make region writable.
START of region.
END of region.
ORIG-BUFF is the original buffer to make writable or readonly.
TMP-BUFF is associated with any overlays created so the overlays can be deleted
when TMP-BUFF is deleted."
  (with-current-buffer orig-buff
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
              (mor--add-overlay-readonly orig-buff tmp-buff start end)
              ;; (overlay-put (make-overlay start end) 'face 'mor-readonly-face)
              (add-text-properties start-adj end '(read-only t)))
          ;; else make writable
          (let ((inhibit-read-only t)) ;; Do i need this?
            (remove-text-properties start-adj end '(read-only t))
            ;; (remove-overlays start end)
            (mor--delete-overlay-readonly orig-buff tmp-buff))))
      (set-buffer-modified-p modified))))

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
                           ;; use dynamically bound value if available.
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

  ;; Using `cl-defun' for the `cl-return-from' feature. An early return feels
  ;; better than nesting code in a conditional statement.
  (cl-defun mor--mode-on-region (start end mode-fn)
    "The core function to copy region to a new buffer.
Region is between START and END.
MODE-FN the function to turn on the desired mode."

    ;; GUARD: Don't allow the new region to overlap another mor region.
    (let ((orig-buff (current-buffer)))
      ;; only loop over tmp buffers belonging to the current orig-buff
      (dolist (b (cl-remove-if-not (lambda (buff)
                                     (eq orig-buff
                                         (with-current-buffer buff
                                           mor--orig-buffer)))
                                   (mor-get-tmp-buffers)))
        (with-current-buffer b
          ;; TODO: fix off-by-1 issue where it wrongly detects overlap immediately
          ;; after an existing region. But detects wrongly on the side of safety
          ;; (overlap is prevented) so no rush to fix.
          (when (mor--overlap-p start end
                                (marker-position mor--start)
                                (marker-position mor--end))
            ;; return early. Overlaps an existing mor region.
            (message "Overlap with another mor region detected. Abort!")
            (cl-return-from mor--mode-on-region)))))


    ;; remember the mode for `mor-prev-mode-on-region'
    (setq mor--prev-mode-fn mode-fn)

    ;; save buffer and region coordinates to copy the text back in later.
    (let ((orig-buff (current-buffer))
          (tmp-buff (get-buffer-create (mor--gen-buffer-name))))


      (kill-ring-save start end) ;; copy highlighted text


      (when mor-readonly-for-extra-protection-p
        ;; GUARD: can't make it readonly if the buffer is already readonly.
        (unless buffer-read-only
          ;; lock down region in `orig-buff' until `tmp-buff' is killed
          (mor--set-region 'readonly start end orig-buff tmp-buff)))

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

      (mor-tmp-buffer-mode) ; for key binds.

      ;; show a header with useful key bind info. Like `org-src-mode' does.
      (set (make-local-variable 'header-line-format)
           (substitute-command-keys
            "[Copy back]: \\[mor-copy-back]  [Abort]: \\[mor-close-tmp-buffer]"))

      (when mor-format-automatically-p
        (indent-region (point-min) (point-max)))

      (when mor-fix-whitespace-p
        (whitespace-cleanup)))))

(defun mor-copy-back ()
  "Copy the tmp buffer text back the original buffer.

WARNING:
Overwrites the original text."
  (interactive)
  (cond
   ;; guard 1. validate we are in a mor-tmp buffer
   ((null mor--orig-buffer)
    (message "You must be in a mor-tmp buffer for this to work."))
   ;; guard 2. ensure orig-buff is not in read-only mode
   ((with-current-buffer mor--orig-buffer buffer-read-only)
    (message "Original buffer is read-only. Cannot copy back."))
   ;; else. Guards passed
   (t
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

      ;; kill the tmp buffer because multiple attempts to copy back text
      ;; will be wrong due to the now invalid start/end location. Will need
      ;; to use a better way to track start/end before we can allow the
      ;; tmp buffer to live longer for multiple copies.
      (quit-window t (get-buffer-window tmp-buff))))))

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
            (end (marker-position mor--end))
            (orig-buff mor--orig-buffer)
            (tmp-buff (current-buffer)))
        (with-current-buffer mor--orig-buffer
          ;; GUARD: if the whole buffer was readonly don't bother toggling.
          (unless buffer-read-only
            (mor--set-region 'writable start end orig-buff tmp-buff))))))

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
    (cl-assert (mor--overlap-p 5 20    ; big region
                               10 12)) ; little region completely inside big.
    (cl-assert (mor--overlap-p 10 12  ; little region
                               5 20)) ; big region completely covering little.
    ;; regions touching, but not overlapping
    (cl-assert (not (mor--overlap-p 1 2
                                    3 4)))
    (cl-assert (not (mor--overlap-p 3 4
                                    1 2)))
    ;; overlapping by 1
    (cl-assert (mor--overlap-p 1 2
                               2 3))
    'pass))

;;; mode-on-region.el ends here
