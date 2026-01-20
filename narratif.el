;;; narratif.el --- Narrative minor mode for Org-mode

;; Copyright (C) 2026, jmc

;; Author:   jmc
;; Keywords: interactive-fiction

;;; Commentary:

;;; Code:

;; local-block-beg	: integer. Point at beginning of current section.
;; local-block-end	: integer. Point at end of current section.
;; local-count-clicks	: integer. Number of passages clicked in current section
;; local-count-turns	: integer. How many links clicked (sections + passages)
;; local-section        : list. Current section (headline)
;; local-seen		: list. Sections visited (headline).
;; local-vars		: alist. Key-Value store.
;; local-ast		: list. The full AST of the sory Org-mode file

;; Utilities and ancillaries

(defun narratif--qorg-headline (ast str &optional level)
  (let ((found nil)
	(lvl (or level 1))
	)
    (org-element-map ast 'headline
      (lambda (headline)
	(if (and (null found)
		 (string= str (org-element-property :raw-value headline))
		 (= lvl (org-element-property :level headline)))
	    (setq found headline))))
    found)
  )

(defun trace-in-scratch (node)
  (with-current-buffer "*scratch*"
    (insert (format "TYPE: %s\n" (org-element-type node)))
    (dolist (line
	     (org-element-properties-map
	      (lambda (pname pvalue pnode)
		(format "%s\t%s\n" pname pvalue))
	      node))
      (insert line))))

(defun story-set (key val)
  (setq-local local-vars
	      (append (list (cons key val))
		      (if (assoc key local-vars)
			  (assoc-delete-all key local-vars)
			local-vars))))

(defun story-get (key)
  (cdr (assoc key local-vars)))

(defun story-section (title)
  (narratif--destination-link title))
  

;; Hooks and effects on hyperlinks for sections and passages

(defun narratif--hook-passage (hl)
  (let* ((section-title (org-element-property :raw-value local-section))
	 (headline (narratif--qorg-headline local-ast section-title))
	 (click-hl (narratif--qorg-headline
		    headline
		    (format "@%d" local-count-clicks)
		    2))
	 )
    (when click-hl
      (narratif--append-passage click-hl t)
      (setq-local local-count-clicks (1- local-count-clicks)))
    )
  )

(defun narratif--deactivate-block ()
  (let ((blk (delete-and-extract-region local-block-beg local-block-end))
	)
    (insert
     (with-temp-buffer
       (insert (org-fontify-like-in-org-mode blk))
       (buffer-substring-no-properties (point-min) (point-max))
       )
     )
    )
  )

(defun narratif--deactivate-link (link)
  (let* ((beg (or (org-element-property :contents-begin link)
		  (+ 2 (org-element-property :begin link))))
	 (end (or (org-element-property :contents-end link)
		  (- (org-element-property :end link) 2)))
	 (link-text (buffer-substring-no-properties beg end))
	 )
    (setq link-text
	  (subst-char-in-string ?\] 32 link-text))
    (delete-region
	  (org-element-property :begin link)
	  (org-element-property :end link))
    (insert link-text))
  (setq-local local-block-end (point-max))
  )


;; Appending to narration

(defun narratif--interpret (str)
  (replace-regexp-in-string
   "@{[^}]+}"
   (lambda (match-text)
     ;; (with-current-buffer "*scratch*"
     ;;   (insert match-text)
     ;;   (insert "\n"))
     (format "%s" (eval (read (substring match-text 2 -1))))
     )
   str))

(defun narratif--append-section (headline)
  (goto-char local-block-end)
  (setq-local local-block-beg (point)
	      local-count-clicks 0)
  (insert
   (format "%s\n\n%s"
	   (make-string (frame-width) ?-)
	   (narratif--interpret
	    (org-element-interpret-data
	     (car (org-element-contents headline))))))
  (setq-local local-block-end (point)
	      local-seen (append local-seen (list local-section)))
  (setq-local local-section headline)
  )

(defun narratif--append-passage (headline &optional in-hook)
  (goto-char local-block-end)
  (insert
   (format "\n%s" (narratif--interpret
		   (org-element-interpret-data
		    (org-element-contents headline)))))
  (setq-local local-block-end (point)
	      local-count-clicks (1+ local-count-clicks))
  (unless in-hook (narratif--hook-passage headline))
  )

;; Navigating current section

(defun narratif--next-link ()
  (interactive)
  (let ((found-link nil))
    (org-element-map (org-element-parse-buffer)	'link
      (lambda (link)
	(if (and (null found-link)
		 (> (org-element-property :begin link) (point)))
	    (setq found-link link))))
    (if found-link (goto-char (org-element-property :begin found-link)))
    )
  )

(defun narratif--prev-link ()
  (interactive)
  (let ((found-link nil)
	(done nil)
	)
    (org-element-map (org-element-parse-buffer)	'link
      (lambda (link)
	(if (null done)
	  (if (>= (org-element-property :begin link) (point))
	      (setq done t)
	    (if (< (org-element-property :end link) (point))
		(setq found-link link))))))
	
    (if found-link (goto-char (org-element-property :begin found-link)))
    )
  )
    
;; Following Hyperlinks

(defun narratif--path-parts (path)
  (string-split path "@"))

(defun narratif--action-link (link)
  (let ((action
	 (cadr
	  (narratif--path-parts (org-element-property :path  link)))))
    (when action
      (eval (read action)))))

(defun narratif--destination-link (link &optional section-only)
  (let ((link-found nil)
	(dest
	 (if (stringp link) link
	   (car (narratif--path-parts (org-element-property :path link)))))
	)
    (org-element-map local-ast 'headline
      (lambda (headline)
	(cond
	 ( ;; Link to SECTION: 
	  (and (null link-found)
	       (= 1 (org-element-property :level headline))
	       (string=	dest
			(org-element-property :raw-value headline)))
	  (setq link-found t)
	  ;; (with-current-buffer (get-buffer-create "*NARRATIF*")
	  ;; Deactivate section
	  (narratif--deactivate-block)
	  ;; Execute action part of path, if any
	  (unless (stringp link)
	    (let ((action
		   (cadr
		    (narratif--path-parts (org-element-property :path  link))))
		  )
	      (when action
		(eval (read action)))))
	  ;; Append new section
	  (narratif--append-section headline)
	  ;; )
	  )
	 
	 ( ;; Link to PASSAGE
	  (and (null section-only)
	       (null link-found)
	       (= 2 (org-element-property :level headline))
	       (string=	dest
			(org-element-property :raw-value headline)))
	  (setq link-found t)
	  ;; (with-current-buffer (get-buffer-create "*NARRATIF*")
	  ;; Deactivate passage link
	  (narratif--deactivate-link link)
	  ;; Execute action part of path, if any
	  (unless (stringp link)
	    (let ((action
		   (cadr
		    (narratif--path-parts (org-element-property :path  link))))
		  )
	      (when action
		(eval (read action)))))
	  ;; Append passage
	  (narratif--append-passage headline)
	  ;; )
	  )
	 )
	)
      )
    link-found
    )
  )

(defun narratif--link ()
  "Follow a narratif link.

A narratif link is an org-mode link where :path has two optional parts,
a destination, which is either a section headline (`*' level 1) or a
passage headline (`**' level 2), and an action (an Emacs Lisp sexp),
separated by the special character `@'.

Following a narratif link may result in changing the text in the current
section, appending a new section to the narration, and/or triggering an
operation specified in the action part of the :path.
"
  (interactive)
  (let ((link (org-element-context))
	)
    (cond
     ((string= "fuzzy" (org-element-property :type link))
      (setq-local local-count-turns (1+ local-count-turns))
      (if (string= ""
		   (car
		    (narratif--path-parts (org-element-property :path  link))))
	  ;; Action-only link
	  (narratif--action-link link)
	;; Destination and optional action link
	(narratif--destination-link link)
	)
      ; End of fuzzy org-mode link
      )
     )
    )
  )

;; Set-up and minor mode definition

(defun narratif-init (org-source-buffer)
  (interactive "bSelect Org source buffer: ")
  (with-current-buffer org-source-buffer
    (let* ((ast (org-element-parse-buffer))
	   (beg (car (org-element-contents ast))))
      (with-current-buffer (get-buffer-create "*NARRATIF*")
	(erase-buffer)
	(org-mode)
	(setq-local local-ast ast
		    local-block-beg (point)
		    local-count-turns 0
		    local-vars nil
		    local-seen nil)
	(insert (org-element-interpret-data beg))
	(setq-local local-block-end (point)
		    local-section beg)
	(narratif-org-mode)
	)
      )
    )
  (pop-to-buffer (get-buffer "*NARRATIF*"))
  )


(define-minor-mode narratif-org-mode
  "Toggle narratif-org-mode"
  :init-value nil
  :lighter " Narratif"
  :keymap
  (define-keymap
    :full t
    :suppress 'nodigits
    "<backspace>"	#'narratif--prev-link
    "TAB"		#'narratif--next-link
    "RET"		#'narratif--link))



(provide 'narratif)

;;; narratif.el ends here


