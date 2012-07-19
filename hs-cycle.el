;;; hs-cycle.el --- folding behave like org-mode.

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2012 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

(load-library "hideshow")

;; todo advice & flet
(defun ruby-move-to-block (n)
  (let (start pos done down)
    (setq start (ruby-calculate-indent))
    (setq down (looking-at (if (< n 0) ruby-block-end-re
                             (concat "\\<\\(" ruby-block-beg-re "\\)\\>"))))
    (while (and (not done) (not (if (< n 0) (bobp) (eobp))))
      (forward-line n)
      (cond
       ((looking-at "^\\s *$"))
       ((looking-at "^\\s *#"))
       ((and (> n 0) (looking-at "^=begin\\>"))
        (re-search-forward "^=end\\>"))
       ((and (< n 0) (looking-at "^=end\\>"))
        (re-search-backward "^=begin\\>"))
       (t
        (setq pos (ruby-calculate-indent));; (current-indentation)
        (cond
         ((< start pos)
          (setq down t))
         ((and down (= pos start))
          (setq done t))
         ((> start pos)
          (setq done t)))))
      (if done
          (save-excursion
            (back-to-indentation)
            (if (looking-at (concat "\\<\\(" ruby-block-mid-re "\\)\\>"))
                (setq done nil))))))
  (back-to-indentation))

(add-to-list
 'hs-special-modes-alist
 '(ruby-mode
   "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do" "end" "#"
   ;;"\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
   ;; (lambda (arg) (ruby-end-of-block));emacs wiki-> move-to-block
   ruby-forward-sexp;; khiker ->end-of-block
   ;;ruby-move-to-block;;bookshelf
   nil))
;;hs-forward-sexp raise error in "string"
;;hs-aready-hidden-p raise error when before "("
(dolist
    (hook
     (list 'emacs-lisp-mode-hook
           'c-mode-common-hook
           ;;'c++-mode-hook
           'python-mode-hook
           ;;'espresso-mode-hook
           'javascript-mode-hook
           'ruby-mode-hook
           ))
  (add-hook
   hook
   '(lambda()
      (hs-minor-mode)
      (hs-org/minor-mode)
      ;;(textmate-mode)
      ;; (if (not(eq window-system nil))
      ;; (hideshowvis-enable))
      )
   ))
;; (auto-install-from-url ")https://raw.github.com/secelis/hideshow-org/master/hideshow-org.el")
(require 'hideshow-org)
(require 'newcomment)

(defun hs-cycle:point-at-visual-eol ()
  "Return
ARGS"
  (save-excursion
    (end-of-visual-line)
    (point)))

(defun hs-cycle:point-at-visual-bol ()
  "Return
ARGS"
  (save-excursion
    (beginning-of-visual-line)
    (point)))

(defun hs-cycle:count-overlay (&optional from to)
  (let ((total 0)
        (from (or from (hs-cycle:point-at-visual-bol)))
        (to (or to (hs-cycle:point-at-visual-eol))))
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
    total))

;;(it (:vars ((cmd "(")))
;;hs-already-hidden-p
;;hs-find-block-beginning raise error when after "("

(defun hs-cycle:hs-find-block-beginning ()
  "Reposition point at block-start.
Return point, or nil if original point was not in a block."
  (condition-case err
      (progn
        ;; for block-beginning
        (forward-char)
        (backward-up-list)
        ;; (forward-char)
        (point))
    (scan-error
     nil
     ;;when top level
     )))
"("
;;this is for hs-already-hidden-p
(defun hs-find-block-beginning ()
  (hs-cycle:hs-find-block-beginning)
  )

;;copy from hs-discard-overlays
(defun hs-cycle:count-overlay-block()
  "Count overlay from block beginnig to block end.
Delete hideshow overlays in region defined by FROM and TO.
    Skip \"internal\" overlays if `hs-allow-nesting' is non-ni."
  ;;copy from hs-hide-block
  (save-excursion
    (let ((c-reg (if hs-c-start-regexp (hs-inside-comment-p) nil))
          (from 0)
          (to 0)
          (total 0)
          (parse-sexp-ignore-comments t));; for hs-forward-sexp-func?
      (cond
       ((and c-reg (or (null (nth 0 c-reg))
                       (<= (count-lines (car c-reg) (nth 1 c-reg)) 1)))
        (message "(not enough comment lines to hide)"))
       (c-reg
        (setq from (nth 0 c-reg))
        (setq to (nth 1 c-reg)))
       ((or (and hs-block-start-regexp
                 (looking-at hs-block-start-regexp));; ok? for 2 charactor
            (condition-case err
                (hs-cycle:hs-find-block-beginning)
              (error (beginning-of-visual-line))))
        ;; (hs-hide-block-at-point end c-reg)
        (setq from (point))
        (condition-case err
            (funcall hs-forward-sexp-func 1)
          (error (end-of-visual-line)))
        (setq to (point))
        ;;(overlay-put (make-overlay minp maxp 'face 'lazy-highlight))
        ))
      (hs-cycle:count-overlay from to)
      )))

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
              ;; for block folding
              (and
               (save-excursion
                 ;; copy from hs-hide-level-recursive
                 (when
                     (condition-case err
                         (hs-cycle:hs-find-block-beginning)
                       (error (beginning-of-visual-line)))
                   (setq from (point))
                   (condition-case err
                       (funcall hs-forward-sexp-func 1)
                     (error (end-of-visual-line)))
                   (setq to (point))
                   (and (= (line-number-at-pos pos)
                           (line-number-at-pos from))
                        (not (= (line-number-at-pos from)
                                (line-number-at-pos to)))))))))
         (save-excursion
           (message "hiden?:%s count:%s comment?:%s from:%S to:%S"
                    (hs-already-hidden-p)
                    (hs-cycle:count-overlay-block)
                    bounds-of-comment
                    from
                    to
                    )
           (let ((count (hs-cycle:count-overlay-block)))
             (progn
               (cond
                ((and bounds-of-comment (not (hs-already-hidden-p)))
                 (hs-hide-block) (message "1:C:FOLDED"))
                ((and bounds-of-comment (hs-already-hidden-p))
                 (hs-show-block) (message "2:C:CHILDREN"))
                ((and bounds-of-comment)
                 (hs-show-block) (message "3:C:SUBTREE"))
                ((and (= count 0))
                 (hs-hide-block) (message "1:FOLDED"))
                ((and (= count 1)
                      (hs-already-hidden-p)
                      ;;same line?
                      )
                 (message "%s:%s"
                          (line-number-at-pos
                           (overlay-start
                            (hs-already-hidden-p)))
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

;; forward-sexp-function hs-forward-sexp-func
;; Should change for Algol-ish modes
;; (browse-url "http://d.hatena.ne.jp/kitokitoki/20091220/p1")
(let ((parse-sexp-ignore-comments t))
  (forward-sexp))

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
;; need error handling when hs-c-start-regexp is nil
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
          (message "%d initial:%d hidable:%S" p q hidable)
          ;;------------
          ;; modify for beginning of buffer
          ;;------------
          (when (and (>= (point) q)
                     (or hidable;; (= p q)
                         (not (eq (point-min) q))))
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

(defun visible-buffer-string-no-properties ()
  "Same as `buffer-string', but excludes invisible text."
  (substring-no-properties (visible-buffer-string)))

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

 ;;hoge.
 ;;.
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

(defvar hs-cycle-test-string5
  "void func(void){
a}"
  "c func string for test")

(defvar hs-cycle-test-string6
  "(;;()
)"
  " func string for test")

(defun hs-cycle:comment-or-string-p ()
  (or (nth 3 (syntax-ppss))
      (nth 4 (syntax-ppss))))

;; (defun hs-cycle:re-search-forward (regexp &optional bound noerror count)
;; cannot replace because regexp is block-beginning or comment-beginning
;;   (let (ret)
;;     (setq ret (re-search-forward regexp bound noerror count))
;;     (while (and (hs-cycle:comment-or-string-p)
;;                 ret)
;;       (setq ret (re-search-forward regexp bound noerror count)))
;;     ret
;;     ))

(defun hs-cycle:hs-hide-all ()
  "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the comments."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\("
                       hs-block-start-regexp
                       "\\)"
                       (if hs-hide-comments-when-hiding-all
                           (concat "\\|\\("
                                   hs-c-start-regexp
                                   "\\)")
                         ""))))
       (while (progn
                (unless hs-hide-comments-when-hiding-all
                  (forward-comment (point-max)))
                (re-search-forward re (point-max) t))
         ;;------------
         ;; modify for "(" ;; (
         ;;------------
         (if (and (match-beginning 1)
                  (not (or (nth 3 (syntax-ppss))
                           (nth 4 (syntax-ppss)))))
             ;; we have found a block beginning
             (progn
               (goto-char (match-beginning 1))
               (if hs-hide-all-non-comment-function
                   (funcall hs-hide-all-non-comment-function)
                 (hs-hide-block-at-point t)))
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

(defun hs-forward-sexp (match-data arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-func' w/ ARG.
Original match data is restored upon return."
  (save-match-data
    (set-match-data match-data)
    (goto-char (match-beginning hs-block-start-mdata-select))
    (funcall hs-forward-sexp-func arg)))

(defun hs-cycle:hs-hide-block-at-point (&optional end comment-reg)
    (if (and (null comment-reg)
             (or (nth 3 (syntax-ppss))
                 (nth 4 (syntax-ppss))))
        nil
      (hs-hide-block-at-point end comment-reg)))

;; (defadvice hs-hide-block-at-point (around hs-cycle:hs-hide-block-at-point
;;                                           activate)
;;   (if (or (nth 3 (syntax-ppss))
;;           (nth 4 (syntax-ppss)))
;;       nil
;;     ad-do-it
;;   ))
;; hs-find-block-beginning
(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string6)
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-cycle:hs-find-block-beginning)
          ))
      (expect 1
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string6)
          (goto-char 5)
          (font-lock-fontify-buffer)
          (hs-cycle:hs-find-block-beginning)
          ))
      ;; (expect "void func(void){}"
      ;;   (with-temp-buffer
      ;;     (c-mode)
      ;;     (insert hs-cycle-test-string5)
      ;;     (goto-char 17)
      ;;     (font-lock-fontify-buffer)
      ;;     (hs-cycle)
      ;;     (visible-buffer-substring-no-properties (point-min) (point-max))
      ;;     ))
      ;; (expect "void func(void){}"
      ;;   (with-temp-buffer
      ;;     (c-mode)
      ;;     (insert hs-cycle-test-string5)
      ;;     (goto-char 1)
      ;;     (font-lock-fontify-buffer)
      ;;     (hs-cycle)
      ;;     (visible-buffer-substring-no-properties (point-min) (point-max))
      ;;     ))
      (desc "hs-inside-comment-p")
      (expect '(1 2);; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
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
      (expect 0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (delete-backward-char 1)
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle:count-overlay-block))
        )
      (expect '(8 34)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert hs-cycle-test-string)
          (goto-char 10)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (desc "gg")
      ;; (expect '(13 27);; '(13 . 28)
      ;;   (with-temp-buffer
      ;;     (emacs-lisp-mode)
      ;;     (insert hs-cycle-test-string)
      ;;     (goto-char 19)
      ;;     (font-lock-fontify-buffer)
      ;;     ;; (hs-cycle:inside-comment-p)
      ;;     ))
      (expect '(8 34);; '(13 . 28)
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
      (expect 0
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";)")
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-cycle:count-overlay-block)
          ))
      (expect ";;hoge"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge\n;;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
      (expect ";;hoge"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";;hoge\n;;")
          (goto-char 2)
          (font-lock-fontify-buffer)
          (hs-cycle)
          (visible-buffer-substring-no-properties (point-min) (point-max))
          ))
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
