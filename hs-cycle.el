;;copy from hs-discard-overlays
(defun hs-count-overlay-block()
  "Delete hideshow overlays in region defined by FROM and TO.
Skip \"internal\" overlays if `hs-allow-nesting' is non-ni."
  (let((from 0)
       (to 0)
       (total 0))
    (save-excursion
      (when (hs-find-block-beginning)
        (setq from (point))
        (funcall hs-forward-sexp-func 1)
        (setq to (point))
        ;;(overlay-put (make-overlay minp maxp 'face 'lazy-highlight))
        ))
    (when (< to from)
      (setq from (prog1 to (setq to from))))
    (if hs-allow-nesting
        (let (ov)
          (while (> to (setq from (next-overlay-change from)))
            (when (setq ov (hs-overlay-at from))
              (setq from (overlay-end ov))
              ;; (delete-overlay ov))))
              (setq total (1+ total)))))
      (dolist (ov (overlays-in from to))
        (when (overlay-get ov 'hs)
          ;;(delete-overlay ov)))))
          (setq total (1+ total))
          )))
    total))

;; copy from hs-show-all
(defun my-hs-show-block ()
  "Show everything then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing blocks ...")
	 (let((from 0)
	      (to 0)
	      (hs-allow-nesting nil))
	   (save-excursion
	     (when (hs-find-block-beginning)
	       (setq from (point))
	       (funcall hs-forward-sexp-func 1)
	       (setq to (point))))
	   (hs-discard-overlays from to))
   (message "Showing blocks ... done")
   (run-hooks 'hs-show-hook)))


(defun hs-end-of-line ()
  (interactive)
  (if (hs-already-hidden-p)
      (progn
	(end-of-line)
	)
    (end-of-line)
    ))

(global-set-key (kbd "\C-e") 'hs-end-of-line)
(global-set-key (kbd "\C-e") 'end-of-line)
(global-set-key (kbd "\C-e") 'seq-end)
(ad-deactivate-regexp 'hs-end-of-line)

(defadvice end-of-line (around hs-end-of-line activate)
  (interactive)
  (if (hs-already-hidden-p)
      (progn
	(next-line)
	(beginning-of-line)
	(backward-char)
	)
    ad-do-it
    ))
(ad-deactivate-regexp 'hs-end-of-line)

;;(hs-already-hidden-p)
(defun hs-cycle()
      (hs-life-goes-on
       (interactive)
       (save-excursion
	 (message "hp%s:n%s:com%s" (hs-already-hidden-p) (hs-count-overlay-block)(hs-inside-comment-p))
	 (let ((count (hs-count-overlay-block)))
	   (progn
	     (cond
	      ((and (hs-inside-comment-p) (not(hs-already-hidden-p)))
	       (hs-hide-block))
	      ((and (hs-inside-comment-p) (hs-already-hidden-p))
	       (hs-show-block))
	      ((and (= count 0))
	       (hs-hide-block))
	      ((and (= count 1)
		    (hs-already-hidden-p))
	       (hs-show-block)
	       (hs-hide-level 1))
	      (t (my-hs-show-block)))
	     )))))

(defadvice hs-toggle-hiding (around hs-cycle-or-none activate)
  (interactive)
  (hs-life-goes-on
   (if
       (and
	(or (not transient-mark-mode)
	    (and transient-mark-mode (not mark-active)));;require
	(or (and (hs-inside-comment-p)
		 (= (line-number-at-pos)
		    (line-number-at-pos(car (hs-inside-comment-p)))))
	    (let ((pos (point))
		  from to)
	      (save-excursion
		(save-restriction
		  ;; copy from hs-hide-level-recursive
		  (when (hs-find-block-beginning)
		    (setq from (point))
		    (funcall hs-forward-sexp-func 1)
		    (setq to (point))
		    (and (= (line-number-at-pos pos)
			    (line-number-at-pos from))
			 (not (= (line-number-at-pos from)
				 (line-number-at-pos to))))))))))
       ad-do-it
     ;;(hs-cycle)
     )))
;;(ad-deactivate-regexp "hs-cycle-or-none")
;;(global-set-key "\C-o" 'hs-toggle-hiding)
;;  )
(provide 'hs-cycle)