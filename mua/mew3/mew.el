;; $Id: mew.el,v 1.1 2004/02/28 15:24:09 nabeken Exp $
(define-key mew-summary-mode-map "bm" 'mew-bsfilter-mark)
(define-key mew-summary-mode-map "bM" 'mew-bsfilter-mark-multi)
(define-key mew-summary-mode-map "bs" 'mew-bsfilter-spam)
(define-key mew-summary-mode-map "bS" 'mew-bsfilter-spam-multi)
(define-key mew-summary-mode-map "bc" 'mew-bsfilter-clean)
(define-key mew-summary-mode-map "bC" 'mew-bsfilter-clean-multi)

(setq mew-refile-ctrl-multi nil)

(setq mew-field-spec
      (reverse (append (list (car (reverse mew-field-spec)))
		       '(("^X-Spam-Probability:$" t)
			 ("^X-Spam-Flag:$" t))
		       (cdr (reverse mew-field-spec)))))

(defun mew-bsfilter-cmd-msg (command)
  "Executing an external command specifying this message as an
argument."
  (interactive)
  (mew-summary-goto-message)
  (mew-summary-msg
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-folder fld msg)))
     (while (not (mew-which-exec command))
       (setq command (read-string "Command: ")))
     (message (format "Executing %s for %s..." command msg))
     (call-process command nil nil nil file)
     (message (format "Executing %s for %s...done" command msg)))))

(defun mew-bsfilter-cmd-msgs (command)
  "Executing an external command specifying messages
marked with '@' as arguments."
  (interactive)
  (mew-summary-multi-msgs
   (let ()
     (while (not (mew-which-exec command))
       (setq command (read-string "Command: ")))
     (message (format "Executing %s ..." command))
     (apply 'call-process command nil nil nil FILES)
     (message (format "Executing %s ...done" command)))))


(defun mew-bsfilter-mark ()
  "mew-bsfilter-mark"
  (interactive)
  (mew-bsfilter-cmd-msg "bs_mark")
  (mew-summary-review))

(defun mew-bsfilter-mark-multi ()
  "mew-bsfilter-mark-multi"
  (interactive)
  (mew-bsfilter-cmd-msgs "bs_mark")
  (mew-summary-mark-review))

(defun mew-bsfilter-spam ()
  "mew-bsfilter-spam"
  (interactive)
  (mew-bsfilter-cmd-msg "bs_spam")
  (mew-summary-review))

(defun mew-bsfilter-spam-multi ()
  "mew-bsfilter-spam-multi"
  (interactive)
  (mew-bsfilter-cmd-msgs "bs_spam")
  (mew-summary-mark-review))

(defun mew-bsfilter-clean ()
  "mew-bsfilter-clean"
  (interactive)
  (mew-bsfilter-cmd-msg "bs_clean")
  (mew-summary-review))

(defun mew-bsfilter-clean-multi ()
  "mew-bsfilter-clean-multi"
  (interactive)
  (mew-bsfilter-cmd-msgs "bs_clean")
  (mew-summary-mark-review))
