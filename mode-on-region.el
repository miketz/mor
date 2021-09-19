;;; mode-on-region.el --- Mode on region -*- lexical-binding: t -*-

;;; License: GPL version 3

;;; Keywords: convenience
;;; Package-Requires: ((emacs "24.1"))
;;; Version: 0.1.0
;;; URL: https://github.com/miketz/mor

;;; Commentary:
;;; `mor-mode-on-region' copies a highlighted region to a tmp buffer and
;;; turns on the mode of your choice.
;;; `mor-prev-mode-on-region' does the same thing, but recalls the previously
;;; chosen mode.
;;; `mor-curr-mode-on-region' does the same thing, but defaults to the mode of
;;; the current buffer.
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
;;; (push "/your/chosen/folder" load-path)
;;; (autoload #'mor-mode-on-region "mode-on-region" nil t)
;;; (autoload #'mor-prev-mode-on-region "mode-on-region" nil t)
;;; (autoload #'mor-curr-mode-on-region "mode-on-region" nil t)
;;; ;; Config vars
;;; (setq mor-format-automatically-p nil)
;;; (setq mor-fix-whitespace-p nil)
;;; (setq mor-readonly-for-extra-protection-p t)
;;; (setq mor-allow-tmp-files-p         t
;;;       mor-modes-to-create-tmp-files '(js2-mode js-mode)
;;;       mor-auto-delete-tmp-files-p   t)
;;; ;; Face for the selected region when readonly mode is enabled.
;;; (custom-set-faces `(mor-readonly-face ((t :background "black"
;;;                                           :foreground "red"
;;;                                           :strike-through t))))
;;; ;; Recommended key binds for vanilla Emacs.  Press "C-c m" with text
;;; ;; highlighted.
;;; (global-set-key (kbd "C-c m") #'mor-mode-on-region)
;;; (global-set-key (kbd "C-c .") #'mor-prev-mode-on-region)
;;; (global-set-key (kbd "C-c r") #'mor-curr-mode-on-region)
;;; (with-eval-after-load 'evil
;;;   ;; Recommended key binds for evil users.  Press "m" in visual mode.
;;;   (define-key evil-visual-state-map (kbd "m") #'mor-mode-on-region)
;;;   (define-key evil-visual-state-map (kbd ".") #'mor-prev-mode-on-region)
;;;   (define-key evil-visual-state-map (kbd "r") #'mor-curr-mode-on-region))
;;; (with-eval-after-load 'mode-on-region
;;;   ;; Recommended key binds for the tmp buffer.  Both Vanilla and Evil.
;;;   (define-key mor-tmp-buffer-mode-map (kbd "C-c b") #'mor-copy-back)
;;;   (define-key mor-tmp-buffer-mode-map (kbd "C-c c") #'mor-close-tmp-buffer))


;;; Code:
(require 'cl-lib)
(provide 'cl-macs) ;; for cl-defstruct
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
  "Minor mode to simulate buffer local key binds in mor tmp buffers.
Before this minor mode, tmp buffer funcs were bound globally and
required guards to verify the user was inside a mor tmp buffer.
NOTE: the guards still exist for needed protection.  The minor mode
key binds just help avoid key bind pollution, and reduce the risk of
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
  "When t automatically fix whitespace via `whitespace-cleanup'."
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

(defcustom mor-prev-mode-fn nil
  "The previous mode function used.
This fn variable is used by `mor-prev-mode-on-region' to remember the previous
mode used.
Making the previous fn configurable allows you to have a default value for the
previous mode.  IE you don't have to type out \"emacs-lisp-mode\" the first
time to get a previous mode set."
  :type 'function
  :group 'mode-on-region)

(defcustom mor-tmp-folder "~/mor-temp-files/"
  "Folder to store temporary files."
  :type 'directory
  :group 'mode-on-region)

(defcustom mor-modes-to-create-tmp-files '()
  "A list of modes for which a tmp files will be created for the tmp buffer.
If you don't know the symbol name of a mode, inspect the buffer local
variable `major-mode' while in a buffer having that major mode active."
  :type 'list
  :group 'mode-on-region)

(defcustom mor-allow-tmp-files-p nil
  "When t allow tmp files to be created.
Tmp files will be created in folder `mor-tmp-folder' for the tmp buffer when
editing in a mode in `mor-modes-to-create-tmp-files'.
Basically this variable exists so you don't need to wipe out the list
of modes to turn off the temp file creation."
  :type 'boolean
  :group 'mode-on-region)

(defcustom mor-auto-delete-tmp-files-p t
  "When t automatically delete tmp files.
The deletion will occur when the associated tmp buffer is killed."
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
(declare-function mor--gen-buffer-name "mode-on-region" ())
(declare-function mor--gen-file-name "mode-on-region" ())

;; TODO: Fix bug where tmp buffer won't die if the orig buffer is killed first.
;;       Probably need some guards when attempting to dispose markers in the
;;       no-longer-existing orig buff.
;; TODO: fix bug where read-only properties are never removed if tmp buffer
;;       mode is manually changed.
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
;; DONE: Optionally create a tmp file on disk. Useful for features that
;;       require a file on disk (some linters, etc).


;; NOTE: moving away from buffer-local variables as they get wiped out when
;; the major mode changes. Opens up many holes where read-only sections and
;; overlays are left orphaned and not cleaned up. (MAJOR BUG!!!)
;; Replacing buffer local vars with a list of global structures.
(cl-defstruct (mor-sel (:constructor mor-sel-create)
                       (:copier nil))
  "Structure to group all relevant info for a selected region.
The markers. The readonly section. References to the relevant buffers. And
anything else."
  (buffer-orig)
  (buffer-tmp)
  ;; markers, overlay exist in buffer-orig
  (marker-start)
  (marker-end)
  (overlays :documentation
            "Association list of overlays in the orig-buffer.
Ties each overlay to the relevant tmp buffer, so the overlay can be deleted
when the tmp-buffer is deleted.
Conceptually it would look something like this:
    ((tmp-buff1 . ov1)
     (tmp-buff2 . ov2))")
  ;; `mor-readonly-for-extra-protection-p' has this globally for all regions
  ;; but store here anyway to support future mixing. And in case anyone changes
  ;; the global after selection creation.
  (readonlyp))

(defvar mor-sel-list '()
  "A global list of all selections.  For all buffers.")

(defun mor-get-selections-for-buffer (buff buff-access-fn)
  "Return a list of selections for the tmp buffer TMP-BUFF.
Using the cl-defstruct acessor BUFF-ACCESS-FN."
  (cl-remove-if-not (lambda (sel)
                      (eq buff
                          (funcall buff-access-fn sel)))
                    mor-sel-list))

(defun mor-get-selections-for-buffer-orig (orig-buff)
  "Return a list of selections for the original buffer ORIG-BUFF.
The user can have multiple selections on the orig buff so this returns a list"
  (mor-get-selections-for-buffer orig-buff
                                 #'mor-sel-buffer-orig))

(defun mor-get-selection-for-buffer-tmp (tmp-buff)
  "Return selection for TMP-BUFF.
Each tmp buffer handles exactly 1 selection so this returns 1 selection.
Returns nil if not found."
  (car (mor-get-selections-for-buffer tmp-buff
                                      #'mor-sel-buffer-tmp)))





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


(defun mor-get-tmp-buffers () ;;###ported
  "Return a list of the mor tmp buffers."
  (cl-loop for b in (mapcar #'mor-sel-buffer-tmp mor-sel-list)
           when (buffer-live-p b) ;;TODO: remove old sel from global list.
           collect b))

;; `mor--prefix' used for tmp buffer names. Make it private by let-binding it
;; and accessing it with lexical scope.
(let ((mor--prefix "mor-tmp-"))
  ;; `seq' is a sequential counter used to generate unique names for tmp
  ;; buffers. Make it private by let-binding it and accessing it with lexical
  ;; scope.
  (let ((seq 0))
    (defun mor--gen-buffer-name () ;;###ported
      "Generate a unique buffer name."
      (prog1
          (concat mor--prefix
                  (buffer-name (current-buffer))
                  "-"
                  (int-to-string seq)
                  "-"
                  (md5 (int-to-string (random))))
        (cl-incf seq))))

  (let ((seq 0))
    (defun mor--gen-file-name () ;;###ported
      "Generate a unique name for a file.
Initially the buffer name was re-used as the file name.
However buffers may have illegal characters that break filenames (especially
on MS-Windows). So for now just make the name from an arbitrary sequence,
not even tied to the buffer name sequence."
      (prog1
          (concat mor--prefix
                  (int-to-string seq)
                  "-"
                  (md5 (int-to-string (random))))
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


(defun mor--overlap-p (start1 end1 start2 end2) ;;###ported
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

(defun mor--set-region (state sel) ;;###ported
  "Make region writable or readonly based on STATE.
If STATE=readonly make region readonly.
If STATE=writable make region writable.
SEL is the selected region object."
  (let ((start (marker-position (mor-sel-marker-start sel)))
        (end (marker-position (mor-sel-marker-end sel)))
        (orig-buff (mor-sel-buffer-orig sel))
        (tmp-buff (mor-sel-buffer-tmp sel)))
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
                ;; create overlay and save back to the struct
                (setf (mor-sel-overlay sel)
                      (mor--add-overlay-readonly orig-buff tmp-buff start end))
                ;; (overlay-put (make-overlay start end) 'face 'mor-readonly-face)
                (add-text-properties start-adj end '(read-only t)))
            ;; else make writable
            (let ((inhibit-read-only t)) ;; Do i need this?
              (remove-text-properties start-adj end '(read-only t))
              ;; (remove-overlays start end)
              (mor--delete-overlay-readonly orig-buff tmp-buff))))
        (set-buffer-modified-p modified)))))

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

;;;###autoload
(defun mor-curr-mode-on-region (start end)
  "Same as `mor-mode-on-region' but default to the mode of the current buffer.
Region is between START and END inclusive."
  (interactive "r")
  (mor--mode-on-region start
                       end
                       ;; TODO: confirm the symbol stored in `major-mode' will
                       ;; always be the name of the function to turn on the
                       ;; major mode. By convention it seems to be.
                       major-mode))


;;;###autoload
(defun mor-prev-mode-on-region (start end)
  "Same as `mor-mode-on-region' but use the previous mode.
Previous mode is saved in variable `mor-prev-mode-fn'.
Region is between START and END inclusive."
  (interactive "r")
  (if (null mor-prev-mode-fn)
      (message "No previously used mode found.")
    (mor--mode-on-region start
                         end
                         mor-prev-mode-fn)))

;; Using `cl-defun' for the `cl-return-from' feature. An early return feels
;; better than nesting code in a conditional statement.
(cl-defun mor--mode-on-region (start end mode-fn) ;;###ported, need more testing
  "The core function to copy region to a new buffer.
Region is between START and END.
MODE-FN the function to turn on the desired mode."

  ;; GUARD: Don't allow the new region to overlap another mor region.
  (let* ((orig-buff (current-buffer))
         (orig-buff-selections (mor-get-selections-for-buffer-orig orig-buff)))
    (dolist (sel orig-buff-selections)
      ;; TODO: fix off-by-1 issue where it wrongly detects overlap immediately
      ;; after an existing region. But detects wrongly on the side of safety
      ;; (overlap is prevented) so no rush to fix.
      (when (mor--overlap-p start end
                            (marker-position (mor-sel-marker-start sel))
                            (marker-position (mor-sel-marker-end sel)))
        ;; return early. Overlaps an existing mor region.
        (message "Overlap with another mor region detected. Abort!")
        (cl-return-from mor--mode-on-region))))


  ;; remember the mode for `mor-prev-mode-on-region'
  (setq mor-prev-mode-fn mode-fn)


  (let ((sel (mor-sel-create
              :buffer-orig (current-buffer)
              :buffer-tmp (get-buffer-create (mor--gen-buffer-name))
              ;; track start/end with markers. Markers will automatically "move"
              ;; as the text around them is changed.
              :marker-start (set-marker (make-marker) start (current-buffer))
              :marker-end (set-marker (make-marker) end (current-buffer))
              :overlays nil ;; will set a bit later
              :readonlyp mor-readonly-for-extra-protection-p)))

    (push sel mor-sel-list) ;; add to global list

    (kill-ring-save start end) ;; copy highlighted text


    (when mor-readonly-for-extra-protection-p
      ;; GUARD: can't make it readonly if the buffer is already readonly.
      (unless buffer-read-only
        ;; lock down region in `orig-buff' until `tmp-buff' is killed
        ;; TODO: look into using markers for start/end of read only region
        (mor--set-region 'readonly sel)))

    (deactivate-mark)

    (funcall mor-switch-buff-fn (mor-sel-buffer-tmp sel))
    (yank)              ;; paste text
    ;; ignore errors before turning on mode, otherwise mor key binds won't be
    ;; set. Like "C-c c" to close.
    (with-demoted-errors "Error: %S"
      (funcall mode-fn)) ;; turn on the dedicated mode.

    (mor-tmp-buffer-mode) ; for key binds.

    ;; Show a header with useful key bind info. Like `org-src-mode' does.
    ;; Also show info about the mode itself in the header.
    (set (make-local-variable 'header-line-format)
         (substitute-command-keys
          (format
           "[Copy back]: \\[mor-copy-back]  [Abort]: \\[mor-close-tmp-buffer]           %s, %s"
           ;; human friendly name of mode
           (if (listp mode-name)
               (car mode-name)
             mode-name)
           ;; name of mode-fn symbol itself. This is sometimes more descriptive
           ;; than the "human friendly" mode-name.
           (symbol-name mode-fn))))

    (when mor-format-automatically-p
      (indent-region (point-min) (point-max)))

    (when mor-fix-whitespace-p
      (whitespace-cleanup))

    (when (and mor-allow-tmp-files-p
               (memq major-mode mor-modes-to-create-tmp-files))
      ;; create folder for tmp files.
      (when (and (not (null mor-tmp-folder))
                 (not (file-exists-p mor-tmp-folder)))
        (make-directory mor-tmp-folder))
      ;; create tmp file.
      (let ((file (concat mor-tmp-folder (mor--gen-file-name))))
        (when (and (not (null file))
                   (file-exists-p file))
          (delete-file file))
        (write-file file)))))

(defun mor-copy-back () ;;###ported, untested
  "Copy the tmp buffer text back the original buffer.

WARNING:
Overwrites the original text."
  (interactive)
  (let* ((sel (mor-get-selection-for-buffer-tmp (current-buffer)))
         (orig-buff (mor-sel-buffer-orig sel)))
    (cond
     ;; guard 1. validate we are in a mor-tmp buffer
     ((null sel)
      (message "You must be in a mor-tmp buffer for this to work."))
     ;; guard 2. ensure orig-buff is not in read-only mode
     ((with-current-buffer orig-buff buffer-read-only)
      (message "Original buffer is read-only. Cannot copy back."))
     ;; else. Guards passed
     (t
      ;; Cache tmp buffer local values. They will be invisible once we switch
      ;; back to the orig buffer.
      (let* ((tmp-buff (mor-sel-buffer-tmp sel))
             (marker-start (mor-sel-marker-start sel))
             (marker-end (mor-sel-marker-end sel))
             (start (marker-position marker-start))
             (rng (- (marker-position marker-end)
                     start)))

        (mor--clean-up)

        ;; copy tmp buffer text.
        (kill-ring-save (point-min) (point-max))

        ;; Switch to orig buff. Buffer local values will be invisible!
        (funcall mor-switch-buff-fn orig-buff)

        ;; delete original selected region
        (goto-char start)
        (delete-char rng)
        ;; paste new text
        (yank)

        ;; kill the tmp buffer because multiple attempts to copy back text
        ;; will be wrong due to the now invalid start/end location. Will need
        ;; to use a better way to track start/end before we can allow the
        ;; tmp buffer to live longer for multiple copies.
        (with-current-buffer tmp-buff
          (set-buffer-modified-p nil) ; avoid save prompt for tmp buffers with tmp files.
          (quit-window t (get-buffer-window tmp-buff))))))))

(defun mor-close-tmp-buffer ()
  "Kill the tmp buffer and clean up the window if applicable.
Call this if you don't want to copy the text back to the original buffer."
  (interactive)
  (set-buffer-modified-p nil) ; avoid save prompt for tmp buffers with tmp files.
  ;; NOTE: relies on `mor--clean-up' for cleanup. via kill-buffer-hook.
  (quit-window t))


(defun mor--marker-active-p (m) ;;###ported
  "Return t if the marker is actively pointing to a position.
M for marker."
  (and (not (null m))
       (not (null (marker-position m)))))

(defun mor--clean-up () ;;###ported, untested
  "Perform cleanup when the tmp buffer is killed.
Unlocks the region if the original buffer.
Deletes a temporary file created for the tmp buffer."
  ;; extract struct members into local variables. For shortness/readability.
  ;; TODO: verify (current-buffer) always refers to tmp buffer. even if buffer kill occured from
  ;; a different buffer like ibuffer.
  (let* ((tmp-buff (current-buffer))
         (sel (mor-get-selection-for-buffer-tmp tmp-buff))
         (readonlyp (mor-sel-readonlyp sel))
         (marker-start (mor-sel-marker-start sel))
         (marker-end (mor-sel-marker-end sel))
         (orig-buff (mor-sel-buffer-orig sel)))
    ;; Unlock region in the original buffer.
    (when readonlyp ;mor-readonly-for-extra-protection-p
      ;; guard against dupe call from hook
      (when (and (mor--marker-active-p marker-start)
                 (mor--marker-active-p marker-end))
        (let ((start (marker-position marker-start))
              (end (marker-position marker-end)))
          (with-current-buffer orig-buff
            ;; GUARD: if the whole buffer was readonly don't bother toggling.
            (unless buffer-read-only
              ;; (mor--set-region 'writable start end orig-buff tmp-buff)
              (mor--set-region 'writable sel))))))

    ;; clear markers
    (when (mor--marker-active-p marker-start) ; guard against dupe call from hook
      (set-marker marker-start nil))
    (when (mor--marker-active-p marker-end) ; guard against dupe call from hook
      (set-marker marker-end nil))

    ;;  selection from the global list
    (setq mor-sel-list (delq sel mor-sel-list)))

  ;; delete tmp file. No guard on `mor-allow-tmp-files-p' because it may
  ;; have been toggled off during the tmp file's lifetime.
  (when mor-auto-delete-tmp-files-p
    (let ((tmp-file (buffer-file-name))) ;;TODO: verify curret buffer is alwyas the tmp buffer
      (when (and (not (null tmp-file))
                 (file-exists-p tmp-file))
        (delete-file tmp-file)))))


;; use a hook to unlock the orig buffer when the tmp buffer is killed
(add-hook 'mor-tmp-buffer-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook
                      'mor--clean-up
                      nil
                      'make-it-local)))


(provide 'mode-on-region)

;;; mode-on-region.el ends here
