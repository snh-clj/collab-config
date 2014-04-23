;;; Keep Evil settings early to minimize pain in case of failure
;;;
;; Evil settings (pre)
(setq evil-want-C-u-scroll t)
;; Adhere to the party line
(add-to-list 'load-path "~/.emacs.d/")
(require 'pair)
;; Uncomment the below line if you don't like vim keybindings
(setq evil-default-state 'emacs)
(evil-mode 1)
;; Evil settings (post)
(define-key evil-motion-state-map " " 'evil-scroll-down)
;;;
;;; End Evil settings

;;(setq meta-flag 1)
(set-input-mode 't nil 't 7) ; 8-bit Meta
;(set-input-mode 't nil 0 7) ; ESC+ Meta

(setq nrepl-popup-stacktraces nil)
(setq inferior-lisp-program "lein repl")

(define-clojure-indent (as-> 1))

; From http://www.emacswiki.org/emacs/BackupDirectory
(if (featurep 'backup-dir)
    (require 'backup-dir)
    ;; localize it for safety.
    (make-variable-buffer-local 'backup-inhibited)
    (setq bkup-backup-directory-info
        '((t "~/.emacs_common/backup" ok-create full-path prepend-name)))
    (setq delete-old-versions t
        kept-old-versions 1
        kept-new-versions 3
                version-control t))

;; Color adjustments (primarily for terminals.
(if window-system
    (progn ; Non-terminal adjustments
        )

    (progn ; Terminal adjustments
        ;; The light colored hl-line is fine for GUI emacs but makes
        ;; it impossible to read things in a white-on-black terminal
        (defadvice hl-line-mode (after
                                 abrooks-advise-hl-line-mode
                                 activate compile)
          (set-face-background hl-line-face "white"))

        ;; This is the coloring of the selection region
        (face-spec-set 'region
                       '((((class color) (background light))
                          (;:foreground "blue"
                           :background "color-17"
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; The idle-highlight is used by starter-kit... I think.
        (face-spec-set 'idle-highlight
                       '((((class color) (background light))
                          (;:foreground "black"
                           :background "gray25"
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; The idle-highlight is used by starter-kit... I think.
        (face-spec-set 'show-paren-match
                       '((((class color) (background light))
                          (:foreground "yellow"
                           :background nil
                           :bold t
                           :strike-through nil
                           :underline nil))
                         (t (:foreground "purple"
                             :background "unspecified"
                             :strike-through nil
                             :underline t))))
        ;; This makes our trailing whitespace actually visible
        (face-spec-set 'trailing-whitespace
                       '((((class color) (background light))
                          (;:foreground "red"
                           :background "red"
                           :strike-through nil
                           :underline nil))
                         (t (;:foreground "red"
                             :background "red"
                             :strike-through nil
                             :underline nil))))))

(add-to-list 'auto-mode-alist '("\\.txn$" . clojure-mode))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

(defmacro after (mode &rest body)
  "After MODE loads, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun cider-send-dwim ()
  "Send the appropriate forms to the REPL to be evaluated."
  (interactive)
  (let ((expr (cider-last-sexp)))
    (pop-to-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert expr)
    (cider-repl-return)
    (other-window -1)))

(after 'cider
  (define-key cider-mode-map (kbd "C-c C-c") 'cider-send-dwim))
