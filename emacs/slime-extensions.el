;;; -*- lexical-binding: t -*-

;; from http://bc.tech.coop/blog/070424.html
;; extended for nREPL

;; fixes/differences
;;
;; - "\s)" and "\s(" needed to be "\\s)" and "\\s(" respectively
;; - made "send and evaluate" the default functionality.  If the
;;   prefix arg is added, it will just send the sexp to the REPL
;; - check at/after before at/before
;; - ignore-errors when moving backward in at/after

;; What DWIM means:
;;
;; 1. If a region has been selected, the region will be copied to the
;;    REPL.
;; 2. If the point is either at a close paren or after a close paren,
;;    the preceeding s-expression will be copied to the REPL.
;; 3. If the point is either at an open paren or before an open paren,
;;    the following s-expression will be copied to the REPL.
;; 4. If none of the above conditions has been met, the top-level
;;    s-expression enclosing the point will be copied to the REPL.
(defun repl-send-dwim-fn (switch-to-repl-fn repl-return-fn)
  "Send the appropriate forms to REPL to be evaluated."
  (lambda (arg)
    (interactive "P")
    (save-excursion
      (cond
       ;;Region selected - evaluate region
       ((not (equal mark-active nil))
        (copy-region-as-kill (mark) (point)))
       ;; At/after sexp - evaluate last sexp
       ((or (looking-at "\\s)")
            (save-excursion
              (ignore-errors (backward-char 1))
              (looking-at "\\s)")))
        (if (looking-at "\\s)")
            (forward-char 1))
        (let ((end (point))
              (beg (save-excursion
                     (backward-list 1)
                     (point))))
          (copy-region-as-kill beg end)))
       ;; At/before sexp - evaluate next sexp
       ((or (looking-at "\\s(")
            (save-excursion
              (ignore-errors (forward-char 1))
              (looking-at "\\s(")))
        (forward-list 1)
        (let ((end (point))
              (beg (save-excursion
                     (backward-list 1)
                     (point))))
          (copy-region-as-kill beg end)))
       ;; Default - evaluate enclosing top-level sexp
       (t (progn
            (while (ignore-errors (progn
                                    (backward-up-list)
                                    t)))
            (forward-list 1)
            (let ((end (point))
                  (beg (save-excursion
                         (backward-list 1)
                         (point))))
              (copy-region-as-kill beg end)))))
      (let ((current-window (selected-window)))
        (funcall switch-to-repl-fn)
        (goto-char (point-max))
        (yank)
        (unless arg (funcall repl-return-fn))
        (select-window current-window)))))

(fset 'slime-send-dwim
      (repl-send-dwim-fn (lambda ()
                           (set-buffer (slime-output-buffer))
                           (unless (eq (current-buffer) (window-buffer))
                             (pop-to-buffer (current-buffer) t)))
                         'slime-repl-return))

(fset 'nrepl-send-dwim
      (repl-send-dwim-fn (lambda ()
                           (condition-case nil
                               (nrepl-switch-to-repl-buffer nil)
                             (error (nrepl-switch-to-repl-buffer))))
                         'nrepl-return))

(fset 'cider-send-dwim
      (repl-send-dwim-fn (lambda ()
                           (condition-case nil
                               (cider-switch-to-repl-buffer nil)
                             (error (cider-switch-to-repl-buffer))))
                         'cider-repl-return))

(provide 'slime-extensions)
