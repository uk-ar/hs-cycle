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

(require 'hideshow)
(make-variable-buffer-local 'hs-block-end-regexp)

(require 'ruby-mode)

(defmacro hs-cycle:save-original-func (symbol)
  `(if (not (fboundp ',(intern (format "%s-org" symbol))))
       (fset ',(intern (format "%s-org" symbol))
             (symbol-function ',symbol))
     (warn "2nd time")
     )
  )

(hs-cycle:save-original-func ruby-move-to-block)

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
   "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do" "end"
   "#"
   ruby-forward-sexp
   nil))

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

;; when emacs version < 24.1 only
;; bug fix - ignore comment or string
;; copy from emacs 24.1
(when
    (version-list-< `(,emacs-major-version ,emacs-minor-version) '(24 1))

  (defun hs-looking-at-block-start-p ()
    "Return non-nil if the point is at the block start."
    (and (looking-at hs-block-start-regexp)
         (save-match-data (not (nth 8 (syntax-ppss))))))

  (hs-cycle:save-original-func hs-find-block-beginning)
  (defun hs-find-block-beginning ()
    "Reposition point at block-start.
Return point, or nil if original point was not in a block."
    (let ((done nil)
          (here (point)))
      ;; look if current line is block start
      (if (hs-looking-at-block-start-p)
          (point)
        ;; look backward for the start of a block that contains the cursor
        (while (and (re-search-backward hs-block-start-regexp nil t)
                    ;; go again if in a comment or a string
                    (or (save-match-data (nth 8 (syntax-ppss)))
                        (not (setq done
                                   (< here (save-excursion
                                             (hs-forward-sexp (match-data t) 1)
                                             (point))))))))
        (if done
            (point)
          (goto-char here)
          nil))))
  )

(hs-cycle:save-original-func hs-hide-block-at-point)

;; modify from emacs 24.1
;; fix for no space
(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block if on block beginning.
Optional arg END means reposition at end.
Optional arg COMMENT-REG is a list of the form (BEGIN END) and
specifies the limits of the comment, or nil if the block is not
a comment.

The block beginning is adjusted by `hs-adjust-block-beginning'
and then further adjusted to be at the end of the line."
  (if comment-reg
      (hs-hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (when (hs-looking-at-block-start-p)
      (let ((mdata (match-data t))
            (header-end (match-end 0))
            p q ov)
        ;; `p' is the point at the end of the block beginning, which
        ;; may need to be adjusted
        (save-excursion
          (if hs-adjust-block-beginning
              (goto-char (funcall hs-adjust-block-beginning
                                  header-end))
            (goto-char header-end))
          (setq p (line-end-position)))
        ;; `q' is the point at the end of the block
        (hs-forward-sexp mdata 1)
        (setq q (if (looking-back hs-block-end-regexp)
                    (match-beginning 0)
                  (point)))
        ;;------------
        ;; modify for (\n)
        ;;------------
        ;; (when (and (< p q) (> (count-lines p q) 1))
        (when (and (< p q) (> (count-lines p (min (1+ q) (point-max))) 1))
          (cond ((and hs-allow-nesting (setq ov (hs-overlay-at p)))
                 (delete-overlay ov))
                ((not hs-allow-nesting)
                 (hs-discard-overlays p q)))
          (hs-make-overlay p q 'code (- header-end p)))
        (goto-char (if end q (min p header-end)))))))

(hs-cycle:save-original-func hs-hide-all)
;; modify from emacs 24.1
;; bug fix - ignore comment or string
(defun hs-hide-all ()
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
                  (not (save-match-data (nth 8 (syntax-ppss)))))
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

;; copy from hs-inside-comment-p
;; return nil when not in comment
(defun hs-cycle:hs-inside-comment-p ()
  (let ((from
         (save-excursion
           ;; copy from delete-blank-lines
           (if (re-search-backward "^[ \t]*$" nil t)
               (progn (forward-line) (point))
             (point-min))
           ))
        (to
         (save-excursion
           (if (re-search-forward "^[ \t]*$" nil t)
               (progn
                 (forward-line)
                 (while (and (looking-at "^[ \t]*$")
                             (not (eobp)))
                   (forward-line))
                 (forward-line -1)
                 (line-end-position))
             (point-max)))))
        ;; (if (re-search-backward "[^ \t\n]" nil t)
        ;;     (progn (forward-line 1) (point))
        ;;   (point-min))
        ;; (forward-sentence))))
    (save-restriction
      (narrow-to-region from to)
      (hs-inside-comment-p))))
;; modify from emacs 24.1
;; Add support for comment
(hs-cycle:save-original-func hs-hide-level-recursive)
(defun hs-hide-level-recursive (arg minp maxp)
  "Recursively hide blocks ARG levels below point in region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((re (concat "\\("
                    hs-block-start-regexp
                    "\\)"
                    (if hs-hide-comments-when-hiding-all
                        (concat "\\|\\("
                                hs-c-start-regexp
                                "\\)")
                      ""))))
    (while (progn
             (unless hs-hide-comments-when-hiding-all
               ;; (point-max)
               (forward-comment maxp))
             ;; (point-max)
             (and (< (point) maxp)
                  (re-search-forward re maxp t)))
      ;;------------
      ;; modify for "(" ;; (
      ;;------------
      (if (and (match-beginning 1)
               (not (save-match-data (nth 8 (syntax-ppss)))))
          ;; we have found a block beginning
          (if (> arg 1)
              (hs-hide-level-recursive (1- arg) minp maxp)
            (goto-char (match-beginning 1))
            (if hs-hide-all-non-comment-function
                (funcall hs-hide-all-non-comment-function)
              (hs-hide-block-at-point t)))
        ;; found a comment, probably
        (let ((c-reg (hs-inside-comment-p)))
          (when (and c-reg (car c-reg))
            (if (> arg 1)
                (hs-hide-level-recursive (1- arg) minp maxp)
            (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                (hs-hide-block-at-point t c-reg)
              (goto-char (nth 1 c-reg)))))))
      ))
  (goto-char maxp)
  )

(hs-cycle:save-original-func hs-inside-comment-p)
;; (fset 'hs-inside-comment-p (symbol-function 'hs-inside-comment-p-org))

;; modify from emacs 24.1
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
    ;; forward comment, and see if we are inside, then extend
    ;; forward and backward as long as we have comments
    (let ((q (point)))
      (skip-chars-forward "[:blank:]")
      ;;------------
      ;; modify for "; ;" comment and at comment in string "";""
      ;;------------
      (let ((state (save-match-data (syntax-ppss))))
        ;; (when (or (looking-at hs-c-start-regexp)
        ;;           (re-search-backward hs-c-start-regexp (point-min) t))
        (when (or (and (nth 4 state) (goto-char (nth 8 state)))
                  (and (looking-at hs-c-start-regexp);; in case of ;;
                       (nth 4 (save-excursion (syntax-ppss (match-end 0))))))
          ;; (while (and (not (bobp))
          ;;             (= (point) (progn (forward-comment -1) (point))))
          ;;   (forward-char -1))
          ;; first get to the beginning of this comment...

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
            (end-of-line)
            (when (>= (point) q)
              (list (and hidable p) (point)))))))))

;; copy from org-special-ctrl-a/e

;; it should use re-search-forward before forward-sexp

;;(it (:vars ((cmd "(")))
;;hs-already-hidden-p
;;hs-find-block-beginning raise error when after "("
;;hs-already-hidden-p raise error when before "("

"("

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
       ((or (hs-find-block-beginning))
        ;; (hs-hide-block-at-point end c-reg)
        (setq from (point))
        (funcall hs-forward-sexp-func 1);; don't save match data
        (setq to (point))
        ;;(overlay-put (make-overlay minp maxp 'face 'lazy-highlight))
        ))
      (hs-cycle:count-overlay from to)
      )))

(defun hs-cycle ()
  (interactive)
  (hs-life-goes-on
   (let* ((pos (point))
          (c-reg (hs-inside-comment-p))
          (bounds-of-comment (if (car c-reg) c-reg nil))
          from
          to)
     (when (and
            (or (not transient-mark-mode)
                (and transient-mark-mode (not mark-active)));;require
            (or
             ;; for comment folding
             (and bounds-of-comment
                  (= (line-number-at-pos pos)
                     (line-number-at-pos (car bounds-of-comment))))
             ;; for block folding
             (and
              (null bounds-of-comment)
              (save-excursion
                ;; copy from hs-hide-level-recursive
                (when (and (hs-find-block-beginning)
                           ;; fresh match-data
                           (looking-at hs-block-start-regexp))
                  (setq from (match-end 0))
                  ;; need error handling?
                  (funcall hs-forward-sexp-func 1);; don't save match data
                  ;; don't save match data in hs-block-start-mdata-select
                  (setq to (if (looking-back hs-block-end-regexp)
                               (match-beginning 0)
                             (point)))
                  (and (= (line-number-at-pos pos)
                          (line-number-at-pos from))
                       (not (= (line-number-at-pos from)
                               (line-number-at-pos to)))))))))
       (save-excursion
         (let ((count (hs-cycle:count-overlay-block)))
           (progn
             (cond
              ((and bounds-of-comment (not (hs-already-hidden-p)))
               (hs-hide-block) (message "1:C:FOLDED"))
              ((and bounds-of-comment (hs-already-hidden-p))
               (hs-show-block) (message "2:C:CHILDREN"))
              ((and bounds-of-comment)
               (hs-show-block) (message "3:C:SUBTREE"))
              ((= count 0)
               (hs-hide-block) (message "1:FOLDED"))
              ;; when top level folded
              ((and (= count 1)
                    (hs-already-hidden-p)
                    (eq (overlay-end (hs-already-hidden-p)) to)
                    ;;same line?
                    )
               (hs-hide-level 1)
               (message "2:CHILDREN"))
              (t
               ;; when children level folded
               (save-restriction
                 ;; (edebug)
                 (message "from:%s:to:%s" from to)
                 (narrow-to-region from to)
                 (hs-show-all))
               (message "3:SUBTREE")))
             )))
       )))
  )

(defcustom hs-cycle:special-ctrl-a/e t
  ""
  :group 'hs-cycle)

(setq hs-cycle:special-ctrl-a/e t)

;; for hs-org
(defadvice hs-toggle-hiding (around hs-cycle:hs-toggle-hiding activate)
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

(defun hs-cycle:comment-or-string-p ()
  ;; syntax-ppss chagnes match-data
  (save-match-data
  (let ((state (syntax-ppss)))
    (or (nth 3 state);; string
        (nth 4 state));; comment
      )))


(defun hs-cycle:forward-block-beginning (arg)
  ;; todo arg,save match
  (condition-case err
      (save-excursion
        (while (and (re-search-forward hs-block-start-regexp nil nil)
                    (hs-cycle:comment-or-string-p)))
        (match-beginning 0))
    (search-failed
     nil)))

(dont-compile
  (when (fboundp 'describe)
    (describe ("hs-cycle" :vars (mode))
      (around
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (funcall mode)
          (funcall el-spec:example)))
      (context ("in emacs-lisp-mode" :vars ((mode 'emacs-lisp-mode)
                                            (string-of-buffer "\
;;a
;;b

\"(\"
;(
\(if
    (a)
    (b))

 ;a
 ;b

"
                                             )))
        ;; todo: add count-lines test-cases
        ;; (it ()
        ;;   (insert string-of-buffer)
        ;;   (should-error (hs-hide-all-org));; bug for infinite loop
        ;;   )
        (it ()
          (insert string-of-buffer)
          (hs-hide-all)
          (should (string= ";;a\n\n\"(\"\n;(\n(if)\n\n ;a\n\n"
                           (visible-buffer-string-no-properties))))
        (context "hs-inside-comment-p"
          (it ()
            (insert ";")
            (should (equal (hs-inside-comment-p) '(1 2))))
          (it ()
            (insert ";")
            (beginning-of-buffer)
            (should (equal (hs-inside-comment-p) '(1 2))))
          (it ()
            (insert ";;")
            (should (equal (hs-inside-comment-p) '(1 3))))
          (it ()
            (insert ";;")
            (goto-char 2)
            (should (equal (hs-inside-comment-p) '(1 3))))
          (it ()
            (insert " ;")
            (beginning-of-buffer)
            (should (equal (hs-inside-comment-p) '(2 3))));; strange spec?
          (it ()
            (insert "a ;")
            (goto-char 2)
            (should (equal (hs-inside-comment-p) '(nil 4))));; strange spec?
          (it ()
            (should (equal (hs-inside-comment-p) nil)))
          (it ()
            (insert " ")
            (should (equal (hs-inside-comment-p) nil)))
          (it ()
            (insert "a")
            (should (equal (hs-inside-comment-p) nil)))
          (it ()
            (insert "a;")
            (should (equal (hs-inside-comment-p) '(nil 3))))
          (it ()
            (c-mode)
            (insert "a/**/")
            (backward-char)
            (should (equal (hs-inside-comment-p) '(nil 6)))
            )
          (it ()
            (c-mode)
            (insert "a/**/ab")
            (goto-char 2)
            (should (equal (hs-inside-comment-p-org) '(nil 8)));; bug?
            )
          (it ()
            (c-mode)
            (insert "a/**/ab")
            (goto-char 2)
            ;; (should (equal (hs-inside-comment-p) '(nil 6)))
            ;; not support
            ;; bug?
            )
          (it ()
            (insert "\na;")
            (should (equal (hs-inside-comment-p-org) nil)));;bug?
          (it ()
            (insert "\na;")
            (should (equal (hs-inside-comment-p) '(nil 4))));;fix
          (it ()
            (insert " ;\n;")
            (should (equal (hs-inside-comment-p) '(2 5))))
          (it ()
            (insert ";\n;")
            (should (equal (hs-inside-comment-p) '(1 4))))
          (it ()
            (insert "; ;")
            (should (equal (hs-inside-comment-p-org) '(3 4))));;bug?
          (it ()
            (insert "; ;")
            (should (equal (hs-inside-comment-p) '(1 4))));;fix
          (it ()
            (insert ";\n")
            (should (equal (hs-inside-comment-p-org) nil)));; ok?
          (it ()
            (insert "a;\n")
            (should (equal (hs-inside-comment-p) nil)));; ok?
          (it ()
            (insert "a;\n;")
            (should (equal (hs-inside-comment-p-org) '(4 5))));; bug?
          (it ()
            (insert "a;\n;")
            ;; (should (equal (hs-inside-comment-p) '(2 5)))
            ;; not support
            );; fix
          (it ()
            (insert "a;\n;")
            (goto-char 2)
            (should (equal (hs-inside-comment-p-org) '(nil 3))));; bug
          (it ()
            (insert "a;\n;")
            (goto-char 2)
            (should (equal (hs-inside-comment-p) '(nil 5))));; fix
          (it ()
            (insert "\";\"")
            (should (equal (hs-inside-comment-p-org) '(nil 4))));; bug
          (it ()
            (insert "\";\"")
            (should (equal (hs-inside-comment-p) nil)));; fix
          (it ()
            (insert "\";\"")
            (goto-char 2)
            (should (equal (hs-inside-comment-p-org) '(nil 4))));; bug
          (it ()
            (insert "\";\"")
            (goto-char 2)
            (should (equal (hs-inside-comment-p) nil)));; fix
          (it ()
            (insert "\
;

;")
            (should (equal (hs-inside-comment-p-org) '(1 5))))
          (it ()
            (insert "\
;

;")
            (should (equal (hs-cycle:hs-inside-comment-p) '(4 5))))
          (it ()
            (insert "\
;

")
            (beginning-of-buffer)
            (should (equal (hs-cycle:hs-inside-comment-p) '(1 2))));; extend
          (it ()
            (insert "\
;;
;;


;;
;;
;;


;;
;;")
            (goto-char 11)
            (should (equal (hs-cycle:hs-inside-comment-p) '(9 17))));; extend
          )
        (context "hs-find-block-beginning"
          (when
              (version-list-<
               `(,emacs-major-version ,emacs-minor-version) '(24 1))
            (it ()
              (insert "\"(\"")
              (should-error (hs-find-block-beginning-org)) ;; bug
              )
            (it ()
              (insert ";(")
              (should-error (hs-find-block-beginning-org)) ;; bug
              )
            (it ()
              (insert "(;(\n)")
              (backward-char)
              (should-not (eq 1 (hs-find-block-beginning-org)));; bug
              )
            )
          (it ()
            (insert "()")
            (should (null (hs-find-block-beginning)))
            )
          (it ()
            (insert "\"(\"")
            (should (null (hs-find-block-beginning))) ;; fix
            )
          (it ()
            (insert ";(")
            (should (null (hs-find-block-beginning))) ;; fix
            )
          (it ()
            (insert "(")
            (should-error (hs-find-block-beginning))
            )
          (it ()
            (insert "(")
            (backward-char)
            (should (eq 1 (hs-find-block-beginning)));; ok?
            )
          (it ()
            (insert ";(\n()")
            (backward-char)
            (should (eq 4 (hs-find-block-beginning)))
            )
          (it ()
            (insert "(;(\n)")
            (backward-char)
            (should (eq 1 (hs-find-block-beginning))) ;; fix
            )
          )
        (context "hs-hide-block-at-point"
          (it ()
            (insert "(\n )")
            (beginning-of-buffer)
            (hs-hide-block-at-point)
            (should (string= "()"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert "()")
            (beginning-of-buffer)
            (hs-hide-block-at-point)
            (should (string= "()"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert "(\n)")
            (beginning-of-buffer)
            (hs-hide-block-at-point-org)
            (should (not (string= "()"
                                  (visible-buffer-string-no-properties))))
            );; bug
          (it ()
            (insert "(\n)")
            (beginning-of-buffer)
            (hs-hide-block-at-point)
            (should (string= "()"
                             (visible-buffer-string-no-properties)))
            )
          )
        (context ("hs-hide-level-recursive"
                  :vars ((string-of-buffer "(
;;a
;;
\(
)
\(
))")))
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-hide-level-recursive 1 nil nil)
            (should (string= "(\n;;a\n()\n())"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-hide-level-recursive-org 1 nil nil)
            (should (string= "(\n;;a\n;;\n()\n())"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-cycle)
            (should (string= "()"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-cycle)
            (hs-cycle)
            (should (string= "(\n;;a\n()\n())"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-cycle)
            (hs-cycle)
            (hs-cycle)
            (should (string= string-of-buffer
                             (visible-buffer-string-no-properties)))
            )
          )
        (context "hs-cycle"
          (context ("1 comment"
                    :vars ((string-of-buffer "\
;;
;;")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= ";;"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("1 comment with pre"
                    :vars ((string-of-buffer "\
a;;
;;")))
            (it ()
              (insert string-of-buffer)
              (goto-char 2)
              (hs-cycle)
              ;; (should (string= "a;;"
              ;;                  (visible-buffer-string-no-properties)))
              ;; not support
              )
            (it ()
              (insert string-of-buffer)
              (goto-char 2)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("with no-indent"
                    :vars ((string-of-buffer "\
\((
)
\(
))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(()"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(()\n())"
                               (visible-buffer-string-no-properties)))
              );; bug?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("with indent" :vars ((string-of-buffer "\
\((
  )
 (
  ))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(()"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(()\n ())"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("with string" :vars ((string-of-buffer "\
\(\"(\"
 )")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(\"(\")"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("with string multi" :vars ((string-of-buffer "\
\(\"(\"
 (
)
 (
))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(\"(\")"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(\"(\"\n ()\n ())"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("with comment multi" :vars ((string-of-buffer "\
\(;(
 (
)
 (
))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(;()"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(;(\n ()\n ())"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("1 element" :vars ((string-of-buffer
                                        "\
\((
))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "(()";; "()"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(())"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          (context ("nested element" :vars ((string-of-buffer
                                        "\
\(
 ((
   )))")))
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (should (string= "()"
                               (visible-buffer-string-no-properties)))
              );; (()) is better?
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (should (string= "(\n (())" ;; "(\n ())"
                               (visible-buffer-string-no-properties)))
              )
            (it ()
              (insert string-of-buffer)
              (beginning-of-buffer)
              (hs-cycle)
              (hs-cycle)
              (hs-cycle)
              (should (string= string-of-buffer
                               (visible-buffer-string-no-properties)))
              )
            )
          )
        )
      (context ("in ruby mode" :vars ((mode 'ruby-mode)
                                      (string-of-buffer "\
#a
#b

\'def\'
#def
def hoge1
   p 'a'
end
def hoge2
p 'b'
end

open('hoge.c') { |f|
  p 'b'
}

 #a
 #b
"
                                       )))
        (context ("no indent" :vars ((string-of-buffer "\
def hoge
p 'a'
end")))
          ;; for ruby-end-of-block & ruby-move-to-block has bug
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (hs-hide-block-at-point t)
            (should (string= "def hogeend"
                             (visible-buffer-string)))
            )
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (ruby-move-to-block-org 1)
            (should-not (eq (point) 16))
            )
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (ruby-move-to-block 1)
            (should (eq (point) 16))
            )
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (hs-hide-block-at-point t)
            ;; (should (string= "def hogeend"
            ;;                  (visible-buffer-string)));;bug
            )
          (it ()
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= "def hogeend"
                             (visible-buffer-string)))
            )
          )
        (context ("multi" :vars ((string-of-buffer "\
def a
end
def b
end")))
          (it ()
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= "\
def aend
def bend"
                             (visible-buffer-string-no-properties)))
            )
          )
        (context ("multi indent" :vars ((string-of-buffer "\
  def a
  end
  def b
  end")))
          (it ()
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= "\
  def aend
  def bend"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (goto-char 3)
            (hs-hide-block-at-point t)
            (should (string= "\
  def aend
  def b
  end"
                             (visible-buffer-string-no-properties)))
            )
  ;;         (it ()
  ;;           (insert string-of-buffer)
  ;;           (goto-char 6)
  ;;           (hs-hide-block-at-point t)
  ;;           (should (string= "\
  ;; def aend
  ;; def b
  ;; end"
  ;;                            (visible-buffer-string-no-properties)))
  ;;           )
          )
        (context ("nested" :vars ((string-of-buffer "\
class Foo
  def a
  end
  def b
  end
end")))
          (it ()
            (insert string-of-buffer)
            (beginning-of-buffer)
            (hs-cycle)
            (hs-cycle)
            (should (string= "\
class Foo
  def aend
  def bend
end"
                             (visible-buffer-string-no-properties)))
            )
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (hs-hide-block-at-point t)
            (should (string= "class Fooend"
                             (visible-buffer-string)))
            )
          (it ()
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= "class Fooend"
                             (visible-buffer-string)))
            )
          )
        (context ("hs-hide-block-at-point" :vars ((string-of-buffer "\
def hoge
  p 'a'
end")))
          (it ()
            (insert string-of-buffer)
            (goto-char 1)
            (hs-hide-block-at-point t)
            (should (string= "def hogeend"
                             (visible-buffer-string)))
            )
          (it ()
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= "def hogeend"
                             (visible-buffer-string)))
            )
          (it ()
            (insert " ")
            (insert string-of-buffer)
            (hs-hide-all)
            (should (string= " def hogeend"
                             (visible-buffer-string)))
            )
          (it ()
            (insert "\"{\"")
            (goto-char 2)
            ;; (should-error (hs-hide-block-at-point-org t))
            )
          (it ()
            (insert ";{")
            (goto-char 2)
            ;; (should-error (hs-hide-block-at-point-org t))
            )
          (it ()
            (insert "\"{\"")
            (goto-char 2)
            (should (eq nil (hs-hide-block-at-point t)))
            )
          (it ()
            (insert ";{")
            (goto-char 2)
            (should (eq nil (hs-hide-block-at-point t)))
            )
          ))
      (context ("in c mode" :vars ((mode 'c-mode)
                                   (string-of-buffer
                                    "void func(void){
a}"
                                    )))
        (it ()
          (insert string-of-buffer)
          (goto-char 16)
          (hs-hide-block-at-point t)
          (should (string= "void func(void){}"
                           (visible-buffer-string)))
          )
        ;; not yet
        ;; (it ()
        ;;   (insert string-of-buffer)
        ;;   (goto-char 1)
        ;;   (hs-hide-block-at-point t)
        ;;   (should (string= "void func(void){}"
        ;;                    (visible-buffer-string)))
        ;;   )
        )
      ))
  )

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

(dont-compile
  (when(fboundp 'expectations)
    (expectations
      (desc "hs-inside-comment-p")
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
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\na;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\nhoge ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge ;")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect nil;; ok
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "hoge")
          (goto-char 1)
          (font-lock-fontify-buffer)
          (hs-inside-comment-p)
          ))
      (expect '(nil 7);; ok
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
      (desc "comment")
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
      (desc "count-overlay")
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
