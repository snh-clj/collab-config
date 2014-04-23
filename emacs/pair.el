;; Setup package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Install standard pair packages if they aren't already:
(condition-case ex
    (require 'clojure-mode)
  ('error
   ;; if we can't load clojure-mode, we probably haven't installed the
   ;; packages we need yet
   (when (string-equal "Y" (read-from-minibuffer "Install standard pair packages? " "Y"))
     (package-refresh-contents)
     (package-install 'paredit)
     (package-install 'starter-kit)
     (package-install 'starter-kit-bindings)
     (package-install 'starter-kit-lisp)
     (package-install 'clojure-mode)
     (package-install 'cider)
     (package-install 'ac-nrepl))))

;; Setup Evil mode
;; Note: Keep evil early to minimize pain in case of failure.

;; Pre-evil-load settings

;; Make C-u scroll up a page as in vim.
(setq evil-want-C-u-scroll t)

;; Define a Clojure style word. (should be limited to .clj files at
;; some point.)
(setq evil-word "-A-Za-z0-9:!#$%&*+<=>?@^_~")

;; Load Evil
(let* ((fname load-file-name)
       (fdir (file-name-directory fname)))
  (add-to-list 'load-path (concat fdir "/evil"))
  (add-to-list 'load-path (concat fdir "/evil/lib")))
(require 'evil)

;; Post-evil-load settings
; ...

;; Setup Paredit mode
(require 'paredit)
;; The below two lines enable better paredit Clojure support including
;; {} paredit matching.
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

(defvar paredit-terminal-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap paredit-mode-map)

    ;; Bizarre terminal sequences for M-right, M-left, C-M-right, &
    ;; C-M-left, respectively.
    (define-key keymap (kbd "ESC <right>") 'paredit-forward-slurp-sexp)
    (define-key keymap (kbd "ESC <left>")  'paredit-forward-barf-sexp)
    (define-key keymap (kbd "ESC M-O d")   'paredit-backward-slurp-sexp)
    (define-key keymap (kbd "ESC M-O c")   'paredit-backward-barf-sexp)

    ;; These are the same as in the regular mode map, except that Emacs
    ;; doesn't recognize the correlation between what the terminal
    ;; sends it and what KBD gives for "<C-up>" &c.)
    (define-key keymap (kbd "ESC O a")   'backward-up-list)
    (define-key keymap (kbd "ESC O b")   'down-list)
    (define-key keymap (kbd "ESC M-O a") 'up-list)
    (define-key keymap (kbd "ESC M-O b") 'backward-down-list)
    (define-key keymap (kbd "ESC M-O c") 'paredit-forward)
    (define-key keymap (kbd "ESC M-O d") 'paredit-backward)
    (define-key keymap (kbd "ESC M-O A")
      'paredit-splice-sexp-killing-backward)
    (define-key keymap (kbd "ESC M-O B")
      'paredit-splice-sexp-killing-forward)

    keymap)
  "Keymap for the paredit minor mode.
Works in `emacs -nw' running under Unix terminals.")

(define-minor-mode paredit-terminal-mode
  "Minor mode for pseudo-structurally editing Lisp code.
Uses alternative keybindings that work in `emacs -nw' running under
Unix terminals.

\\{paredit-terminal-mode-map}"
  :lighter " Paredit(nw)")

;; Redefine esk-turn-on-paredit so that alternate keybindings are used
;; for paredit instead of ones that are impossible to type in a terminal.
(defun esk-turn-on-paredit ()
  (paredit-terminal-mode t))

(defun paredit-duplicate-after-point
  ()
  "Duplicates the content of the line that is after the point."
  (interactive)
  ;; skips to the next sexp
  (while (looking-at " ")
    (forward-char))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun swap-sexp-backwards ()
  "Swaps current sexp with previous, leaving cursor on current sexp."
  (interactive)
  (forward-sexp)
  (transpose-sexps -1)
  (backward-sexp)
  (backward-sexp))

(defun swap-sexp-forwards ()
  "Swaps current sexp with subsequent, leaving cursor on current sexp."
  (interactive)
  (forward-sexp)
  (transpose-sexps 1)
  (backward-sexp))

(defun swap-sexp-pair-backwards ()
  "Swaps current and subsequent sexps with preceeding sexp pair, leaving cursor on current sexp."
  (interactive)
  (point-to-register ?\_)

  ;; Wrap current and previous sexp pairs for use with transpose

  (paredit-wrap-sexp)
  (paredit-forward-slurp-sexp)

  (jump-to-register ?\_)
  (backward-sexp)
  (backward-sexp)

  (paredit-wrap-sexp)
  (paredit-forward-slurp-sexp)

  (jump-to-register ?\_)
  (transpose-sexps 1)

  ;; Splice to remove temporary pair wrapping

  (jump-to-register ?\_)
  (paredit-forward-down)
  (call-interactively 'paredit-splice-sexp)

  (jump-to-register ?\_)
  (backward-sexp)
  (paredit-forward-down)
  (call-interactively 'paredit-splice-sexp))

(defun swap-sexp-pair-forwards ()
  "Swaps current and next sexps with subsequent sexp pair, leaving cursor on current sexp."
  (interactive)
  (point-to-register ?\_)

  ;; Wrap current and subsequent sexp pairs for use with transpose

  (paredit-wrap-sexp)
  (paredit-forward-slurp-sexp)

  (jump-to-register ?\_)
  (forward-sexp)
  (forward-sexp)
  (backward-sexp)

  (paredit-wrap-sexp)
  (paredit-forward-slurp-sexp)

  (paredit-backward-up)
  (transpose-sexps 1)

  ;; Splice to remove temporary pair wrapping

  (jump-to-register ?\_)
  (paredit-forward-down)
  (call-interactively 'paredit-splice-sexp)

  (jump-to-register ?\_)
  (forward-sexp)
  (forward-sexp)
  (paredit-forward-down)
  (call-interactively 'paredit-splice-sexp))

(define-key evil-motion-state-map "\M-<" 'swap-sexp-backwards)
(define-key evil-motion-state-map "\M->" 'swap-sexp-forwards)
(define-key evil-motion-state-map "\M-{" 'swap-sexp-pair-backwards)
(define-key evil-motion-state-map "\M-}" 'swap-sexp-pair-forwards)

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-c d") 'paredit-duplicate-after-point)))

;; Use our slime extensions
;;(require 'pair-slime-extensions)

;;(defun pair-slime-hook-function ()
;;  (local-set-key (kbd "C-c C-s") 'slime-send-dwim))

;;(add-hook 'slime-mode-hook 'pair-slime-hook-function)

;; (defun pair-nrepl-hook-function ()
;;   (local-set-key (kbd "C-c C-s") 'nrepl-send-dwim))

;; (add-hook 'nrepl-interaction-mode-hook 'pair-nrepl-hook-function)

;; Add additional clojure file associations
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.txn$" . clojure-mode))

;; Miscellaneous settings

;; Don't turn "fn" into "ƒ"
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; Don't be one of *those* emacsers (who leave whitespace droppings).
(setq-default show-trailing-whitespace t)

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
          (set-face-background hl-line-face "gray13"))

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
                             :underline nil))))
        ;; Set starter-kit parens
        ;; Defaults to gray50 from the 16-color pallet, so set it to
        ;; a similar color but from the 256-color pallet.
        (face-spec-set 'esk-paren-face
                       '((t (:foreground "grey55"))))))

(provide 'pair)
