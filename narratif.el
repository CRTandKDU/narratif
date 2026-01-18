;;; narratif.el --- Narrative minor mode for Org-mode

;; Copyright (C) 2026, jmc

;; Author:   jmc
;; Keywords: interactive-fiction

;;; Commentary:

;;; Code:

;; local-block-beg
o;; local-block-end
;; local-count-clicks
;; local-ast

(defun narratif--qorg-headline (ast str)
  (let ((found nil))
    (org-element-map ast 'headline
      (lambda (headline)
	(if (and (null found)
		 (string= str (org-element-property :raw-value headline)))
	    (setq found headline))))
    found)
  )

(defun narratif--hook-passage (hl)
  (let* ((section-title (org-element-property :raw-value local-section))
	 (headline (narratif--qorg-headline local-ast section-title))
	 (click-hl (narratif--qorg-headline headline
					    (format "@%d" local-count-clicks)))
	 )
    (when click-hl
      (narratif--append-passage click-hl t)
      (setq-local local-count-clicks (1- local-count-clicks)))
    )
  )

(defun narratif--append-section (headline)
  (goto-char local-block-end)
  (setq-local local-block-beg (point)
	      local-count-clicks 0)
  (insert
   (format "%s\n\n%s"
	   (make-string (frame-width) ?-)
	   (org-element-interpret-data
	    (car (org-element-contents headline)))))
  (setq-local local-block-end (point)
	      local-section headline)
  )

(defun narratif--append-passage (headline &optional in-hook)
  (goto-char local-block-end)
  (insert
   (format "\n%s" (org-element-interpret-data
		   (org-element-contents headline))))
  (setq-local local-block-end (point)
	      local-count-clicks (1+ local-count-clicks))
  (unless in-hook (narratif--hook-passage headline))
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

(defun narratif--link ()
  (interactive)
  (let ((link (org-element-context))
	(link-found nil)
	)
    (cond
     ((string= "fuzzy" (org-element-property :type link))
      (org-element-map local-ast 'headline
	(lambda (headline)
	  (cond
	   ( ;; Link to SECTION
	    (and (null link-found)
		 (= 1 (org-element-property :level headline))
		 (string= (org-element-property :path  link)
			  (org-element-property :raw-value headline)))
	    (setq link-found t)
	    ;; (with-current-buffer (get-buffer-create "*NARRATIF*")
	      ;; Deactivate section
	      (narratif--deactivate-block)
	      ;; Append new section
	      (narratif--append-section headline)
	      ;; )
	    )
	   
	   ( ;; Link to PASSAGE
	    (and (null link-found)
		 (= 2 (org-element-property :level headline))
		 (string= (org-element-property :path  link)
			  (org-element-property :raw-value headline)))
	    (setq link-found t)
	    ;; (with-current-buffer (get-buffer-create "*NARRATIF*")
	      ;; Deactivate passage link
	      (narratif--deactivate-link link)
	      ;; Append passage
	      (narratif--append-passage headline)
	      ;; )
	    )

	   )
	  )
	)
	; End of fuzzy org-mode link
      )
     )
    )
  )

(defun narratif-init (org-source-buffer)
  (interactive "bSelect Org source buffer: ")
  (with-current-buffer org-source-buffer
    (let* ((ast (org-element-parse-buffer))
	   (beg (car (org-element-contents ast))))
      (with-current-buffer (get-buffer-create "*NARRATIF*")
	(erase-buffer)
	(org-mode)
	(setq-local local-ast ast
		    local-block-beg (point))
	(insert (org-element-interpret-data beg))
	(setq-local local-block-end (point)
		    local-section beg)
	(narratif-org-mode)
	)
      )
    )
  (pop-to-buffer (get-buffer "*NARRATIF*"))
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


