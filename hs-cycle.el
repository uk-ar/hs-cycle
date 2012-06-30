(load-library "hideshow")

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do" "end" "#"
               ;;"\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block))
               ;;ruby-move-to-block
               nil))

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c-mode-common-hook
                    ;;'c++-mode-hook
                    'python-mode-hook
                    ;;'espresso-mode-hook
                    'javascript-mode-hook
                    'ruby-mode-hook
                    ))
  (add-hook hook
            '(lambda()
               (hs-minor-mode)
               (hs-org/minor-mode)
               ;;(textmate-mode)
               ;; (if (not(eq window-system nil))
               ;; (hideshowvis-enable))
               )
            ))
;; (auto-install-from-url "https://raw.github.com/secelis/hideshow-org/master/hideshow-org.el")
(require 'hideshow-org)
(require 'newcomment)

;;copy from hs-discard-overlays
(defun hs-cycle:count-overlay-block()
  "Delete hideshow overlays in region defined by FROM and TO.
    Skip \"internal\" overlays if `hs-allow-nesting' is non-ni."
  ;;copy from hs-hide-block
  (save-excursion
    (let ((c-reg (hs-inside-comment-p))
          (from 0)
          (to 0)
          (total 0))
      (cond
       ;; ((and c-reg (or (null (nth 0 c-reg))
       ;;                 (<= (count-lines (car c-reg) (nth 1 c-reg)) 1)))
       ;;  (message "(not enough comment lines to hide)"))
       ((or c-reg
            (looking-at hs-block-start-regexp);; ok? for 2 charactor
            (hs-find-block-beginning))
        ;; (hs-hide-block-at-point end c-reg)
        (setq from (point))
        (funcall hs-forward-sexp-func 1)
        (setq to (point))
        ;;(overlay-put (make-overlay minp maxp 'face 'lazy-highlight))
        ))
      (when (< to from)
        (setq from (prog1 to (setq to from))));; swap
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
      total)))

;;
(defun hs-cycle ()
  (interactive)
  (hs-life-goes-on
   (let* ((pos (point))
          (c-reg (hs-inside-comment-p))
          (bounds-of-comment (if (car c-reg) c-reg nil))
          from
          to)
     (if (and
          (or (not transient-mark-mode)
              (and transient-mark-mode (not mark-active)));;require
          (or (and ;; for comment folding
               ;; (message "%s:%s:%s" (hs-inside-comment-p) (line-number-at-pos)(line-number-at-pos(car (hs-inside-comment-p)))t)
               bounds-of-comment
               (= (line-number-at-pos)
                  (line-number-at-pos (car bounds-of-comment))))
              (and
               (save-excursion
                 ;; copy from hs-hide-level-recursive
                 (when (hs-find-block-beginning)
                   (setq from (point))
                   (funcall hs-forward-sexp-func 1)
                   (setq to (point))
                   (and (= (line-number-at-pos pos)
                           (line-number-at-pos from))
                        (not (= (line-number-at-pos from)
                                (line-number-at-pos to)))))))))
         (save-excursion
           (message "hiden?:%s count:%s comment?:%s"
                    (hs-already-hidden-p)
                    (my-hs-count-overlay-block)
                    bounds-of-comment)
           (let ((count (my-hs-count-overlay-block)))
             (progn
               (cond
                ((and bounds-of-comment (not (hs-already-hidden-p)))
                 (hs-hide-block) (message "1:C:FOLDED"))
                ((and bounds-of-comment (hs-already-hidden-p))
                 (hs-show-block) (message "2:C:SUBTREE"))
                ((and (= count 0))
                 (hs-hide-block) (message "1:FOLDED"))
                ((and (= count 1)
                      (hs-already-hidden-p)
                      ;;same line?
                      )
                 (message "%s:%s"
                          (line-number-at-pos(overlay-start(hs-already-hidden-p)))
                          pos)
                 (hs-show-block)
                 (save-restriction
                   (narrow-to-region (1+ from) (1- to))
                   ;;(hs-show-all)
                   (hs-hide-all))
                 ;;(hs-show-block)
                 ;;(hs-hide-level 1)
                 (message "2:CHILDREN"))
                (t
                 (save-restriction
                   (narrow-to-region (1+ from) (1- to))
                   (hs-show-all))
                 (message "3:SUBTREE")))
               )))
       )))
  )

;; copy from hs-inside-comment-p
;; return nil when not in comment
(defun hs-cycle:inside-comment-p ()
  (let ((from (save-excursion
                ;; (backward-blankline)
                ))
        (to (save-excursion
              (forward-sentence))))
    (save-excursion
      (narrow-to-region from to)
      (hs-inside-comment-p))))

;; (1 2) ";_"
;; (nil 2) ";\n_"
;; nil "_"
;; bug fix for ;; ;; comment
(defun hs-inside-comment-p ()
  "Return non-nil if point is inside a comment, otherwise nil.
Actually, return a list containing the buffer position of the start
and the end of the comment.  A comment block can be hidden only if on
its starting line there is only whitespace preceding the actual comment
beginning.  If we are inside of a comment but this condition is not met,
we return a list having a nil as its car and the end of comment position
as cdr."
  (save-excursion
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend extend
    ;; forward and backward as long as we have comments
    (let ((q (point))
          (hs-cycle:special-ctrl-a/e nil))
      (when (or (looking-at hs-c-start-regexp)
                ;; (re-search-backward hs-c-start-regexp (point-min) t))
                ;;------------
                ;; modify for ;; ;; comment
                ;;------------
                (goto-char (or (comment-beginning) (point-min))))
        ;; first get to the beginning of this comment...
        (while (and (not (bobp))
                    ;; (= (point) (progn (forward-comment -1) (point))))
                    ;;------------
                    ;; modify for extend backward 1
                    ;;------------
                    (not (= (point) (progn (forward-comment -1) (point)))))
          ;;------------
          ;; modify for extend backward 2
          ;;------------
          ;; (forward-char -1)
          )
        ;; ...then extend backwards
        (forward-comment (- (buffer-size)))
        (skip-chars-forward " \t\n\f")
        (let ((p (point))
              (hidable t))
          (beginning-of-line)
          (unless (looking-at (concat "[ \t]*" hs-c-start-regexp))
            ;; we are in this situation: (example)
            ;; (defun bar ()
            ;;      (foo)
            ;;                ) ; comment
            ;;                 ^
            ;;   the point was here before doing (beginning-of-line)
            ;; here we should advance till the next comment which
            ;; eventually has only white spaces preceding it on the same
            ;; line
            (goto-char p)
            (forward-comment 1)
            (skip-chars-forward " \t\n\f")
            (setq p (point))
            (while (and (< (point) q)
                        (> (point) p)
                        (not (looking-at hs-c-start-regexp)))
              ;; avoid an infinite cycle
              (setq p (point))
              (forward-comment 1)
              (skip-chars-forward " \t\n\f"))
            (when (or (not (looking-at hs-c-start-regexp))
                      (> (point) q))
              ;; we cannot hide this comment block
              (setq hidable nil)))
          ;; goto the end of the comment
          (forward-comment (buffer-size))
          (skip-chars-backward " \t\n\f")
          ;;------------
          ;; modify for first line
          ;;------------
          (unless (= (point) (point-min))
            (end-of-line))
          (message "%d:%d" p q)
          ;;------------
          ;; modify for beginning of buffer
          ;;------------
          (when (and (>= (point) q)
                     (not (eq (point-min) q)))
            (list (and hidable p) (point))))))))

;; copy from org-special-ctrl-a/e
(defcustom hs-cycle:special-ctrl-a/e t
  ""
  :group 'hs-cycle)

(setq hs-cycle:special-ctrl-a/e t)

;; aaaa
(defadvice hs-toggle-hiding (around hs-cycle-or-none activate)
  (interactive)
  ;;   ad-do-it
  (hs-cycle)
  )

;; minor-mode is better?
(defadvice end-of-line (around hs-cycle:end-of-line activate)
  "Return
"
  (interactive)
  (if (and hs-cycle:special-ctrl-a/e
           (eq 0 (hs-cycle:count-overlay))
           (not (interactive-p)))
      ad-do-it
    (end-of-visible-line)))

(ad-disable-advice 'end-of-line 'around 'hs-cycle:end-of-line)
(ad-activate 'end-of-line)

(defun visible-buffer-string ()
  "Same as `buffer-string', but excludes invisible text."
  (visible-buffer-substring (point-min) (point-max)))

(defun visible-buffer-substring-no-properties (start end)
  (visible-buffer-substring start end t)
  )

(defun visible-buffer-substring (start end &optional no-propertiesp)
  "Same as `buffer-substring', but excludes invisible text."
  (let ((str)
        (func (if no-propertiesp
                  'buffer-substring-no-properties 'buffer-substring)))
    (while (< start end)
      (let ((next-pos (next-char-property-change start end)))
        (when (not (invisible-p start))
          (setq str (concat str (funcall func start next-pos))))
        (setq start next-pos)))
    str))

(defvar hs-cycle-test-string
  "(cons
 ;;

 ;;hoge
 ;;
 ;;

 ;;
 )"
  "string for test")

(defvar hs-cycle-test-string2
  "(defun hoge ()
  \"docstring\"
  )"
  "defun string for test")

(defvar hs-cycle-test-string3
  "(if (a)
    (if
        a
        b))"
  "if string for test")

(defvar hs-cycle-test-string4
  "(if (if a
        b)
    (if
        a
        b))"
  "if string for test")

(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "hs-cycle")
      (expect '(1 2);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert " ")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 8);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\nhoge ;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 4);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a ;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 3);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 5);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na ;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 4);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "a ;\n")
          ;; (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (desc "hoge")
      (expect nil;; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\nhoge ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 7);; bug
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\nhoge;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 7);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge ;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 6);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge;")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect ";;hoge"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge")
          (save-excursion
            (insert "\n;;"))
          (font-lock-fontify-buffer)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n")
          (insert hs-cycle-test-string2)
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 18)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-cycle:hs-find-block-beginning)
          ))
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle:hs-find-block-beginning)
          ))
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 15);; end of line
          (font-lock-fontify-buffer)
          (hs-cycle:hs-find-block-beginning)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string2)
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(8 32)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (goto-char 10)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(13 28);; '(13 . 28)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (goto-char 19)
          (font-lock-fontify-buffer)
          (my-hs-inside-comment-p)
          ))
      (expect '(8 32);; '(13 . 28)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (goto-char 19)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(1 10)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";; ;;hoge")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(1 10)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge")
          (save-excursion
            (insert "\n;;"))
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(1 11)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge")
          (save-excursion
            (insert "\n\n;;"))
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge\n")
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(1 11)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge\n")
          (save-excursion
            (insert "\n;;"))
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect "(if (a))"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string3)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-hide-block)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      ;; (expect "(if (a))"
      ;;   (with-temp-buffer
      ;;     (emacs-lisp-mode)
      ;;     (insert hs-cycle-test-string3)
      ;;     (goto-char 5)
      ;;     (font-lock-fontify-buffer)
      ;;     (hs-hide-block)
      ;;     (visible-buffer-substring-no-properties (point-min) (point-max))
      ;;     ))
      (expect "(if (a))"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string3)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect "(if (a)\n    (if))"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string3)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect hs-cycle-test-string3
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string3)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (hs-cycle)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect hs-cycle-test-string4
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string4)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (hs-cycle)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string3)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-hide-block)
          (goto-char 1);; hide block moves point
          (hs-cycle:count-overlay-block)
          ))
      )))

;;(ad-deactivate-regexp "hs-cycle-or-none")
;;(global-set-key "\C-o" 'hs-toggle-hiding)
(provide 'hs-cycle)
;;; hs-cycle.el ends here
