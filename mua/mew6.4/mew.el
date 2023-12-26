;; $Id: mew.el,v 1.2 2012/06/17 06:57:31 nabeken Exp $

; moved from .emacs.el
(setq mew-spam: "X-Spam-Flag:")

; put "D"
(defun mew-spam-bsfilter (val)
  (let ((case-fold-search t))
    (if (string-match "yes" val) ?D)))

; put "o +sapm" at inc
;(defun mew-spam-bsfilter (val)
;  (let ((case-fold-search t))
;    (if (string-match "yes" val) "+spam")))

(setq mew-inbox-action-alist
      '(("X-Spam-Flag:" mew-spam-bsfilter)))

; for "ls" (learn-spam)
(setq mew-spam-prog "bsfilter")
(setq mew-spam-prog-args '("-C" "-s" "-u"))

; for "lh" (learn-ham)
(setq mew-ham-prog "bsfilter")
(setq mew-ham-prog-args '("-c" "-S" "-u"))

; for "lm" (mark-spam)
(define-key mew-summary-mode-map "lm" 'mew-summary-bsfilter-mark-region)

(defun mew-summary-bsfilter-mark-region (&optional arg)
  "study/judge the region and put the '*' mark onto spams.
need to re-learn if judgment of bsfilter is wrong"
  (interactive "P")
  (mew-pickable
   (mew-summary-with-mewl
    (let* ((folder (mew-summary-physical-folder))
	   (msgs (mew-summary-pick-msgs folder t))
	   (prog "bsfilter")
	   (opts '("-a" "--list-spam"))
	   (pattern nil))
      (setq msgs (mew-summary-pick-with-grep prog opts pattern folder msgs))
      (mew-summary-pick-ls folder msgs)))))

; show X-Spam-Flag and X-Spam-Probability in message buffer
(setq mew-field-spec
      (reverse (append (list (car (reverse mew-field-spec)))
		       '(("^X-Spam-Probability:$" t)
			 ("^X-Spam-Flag:$" t))
		       (cdr (reverse mew-field-spec)))))

