;; $Id: mew.el,v 1.3 2006/01/14 07:38:50 nabeken Exp $

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

; for "bm" (mark-spam)
(define-key mew-summary-mode-map "bm" 'mew-summary-bsfilter-mark-region)

(defun mew-summary-bsfilter-mark-region (&optional arg)
  "study/judge the region and put the '*' mark onto spams.
need to re-learn if judgment of bsfilter is wrong"
  (interactive "P")
  (mew-pickable
   (let ((func 'mew-summary-pick-with-cmd)
	 (mew-inherit-grep-cmd "bsfilter -a --list-spam"))
     (mew-summary-pick-body func t nil 'nopattern))))

; code for Mew 4.2.53 by emon
; (defun mew-summary-bsfilter-mark-region (&optional arg)
;  "study/judge the region and put the '*' mark onto spams.
; need to re-learn if judgment of bsfilter is wrong.
; press return-key twice at the prompt."
;  (interactive "P")
;  (mew-pickable
;   (let ((mew-prog-grep "bsfilter")
;       (mew-prog-grep-opts '("-a" "--list-spam")))
;     (mew-summary-pick t))))

; show X-Spam-Flag and X-Spam-Probability in message buffer
(setq mew-field-spec
      (reverse (append (list (car (reverse mew-field-spec)))
		       '(("^X-Spam-Probability:$" t)
			 ("^X-Spam-Flag:$" t))
		       (cdr (reverse mew-field-spec)))))

