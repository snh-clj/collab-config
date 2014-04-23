;; Turn off mouse and wheel interface early to avoid momentary display
(menu-bar-mode -1)

;;; Keep Evil settings early to minimize pain in case of failure
;;;
;; Evil settings (pre)
(setq evil-want-C-u-scroll t)
;; Load Evil
(let* ((fname load-file-name)
       (fdir (file-name-directory fname)))
  (add-to-list 'load-path (concat fdir "/evil"))
  (add-to-list 'load-path (concat fdir "/evil/lib")))
(require 'evil)
;; Comment the below line if you want vim keybindings by default
(setq evil-default-state 'emacs)
(evil-mode 1)
;; Evil settings (post)

;; Define a Clojure style word. (should be limited to .clj files at
;; some point.)
;;(setq evil-word "-A-Za-z0-9:!#$%&*+<=>?@^_~")

;;(define-key evil-motion-state-map " " 'evil-scroll-down)

;; Make C-u scroll up a page as in vim.
(setq evil-want-C-u-scroll t)

;;;
;;; End Evil settings

;;----------------------------------------------------------------------------
;;-- packages.init
;;----------------------------------------------------------------------------

;; MEPLA and Marmalade
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
	          '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	          '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;----------------------------------------------------------------------------
;;-- packages.list
;;----------------------------------------------------------------------------

(defvar user-packages
  '(;; language modes
    clojure-mode
    markdown-mode
    haml-mode
    scss-mode
    scala-mode
    sml-mode
    jade-mode
    yaml-mode
    js2-mode

    ;; slime-like support for scheme
    ;; requires a recent version of racket or guile
    geiser

    ;; interface for Git through Emacs
    ;;; TODO: temporarily use a magit checkout while the git-commit
    ;;; workflow is being worked out
    ;;; TODO: remove (load "lib/magit")
    ;; magit

    ;; git-related modes
    gitconfig-mode
    gitignore-mode

    ;; structured editing for Lisp S-expressions and delimiters
    ;; cheatsheet: http://mumble.net/~campbell/emacs/paredit.html
    paredit

    ;; nested delimiters (parens, brackets, etc.) are colored differently
    rainbow-delimiters

    ;; auto-completion w/ popup box
    auto-complete

    ;; enhanced Ido-mode-like M-x
    smex

    ;; Emacs client for nREPL, an alternative to slime + swank-clojure
    cider

    ;; Node.js REPL
    nodejs-repl

    ;; auto-complete extension for use with nrepl
    ac-nrepl

    ;; the solarized color theme for use with load-theme
    color-theme-solarized

    ;; like 'f' in vim
    iy-go-to-char

    ;; notational velocity-like note taking
    deft

    ;; vimium-like text jumping
    ace-jump-mode

    ;; offline clojure cheatsheet
    clojure-cheatsheet

    ;; fuzzy matching for ido
    flx-ido

    ;; narrowing and selection
    helm))

;;----------------------------------------------------------------------------
;;-- packages.install
;;----------------------------------------------------------------------------

(defun za/install-packages ()
  "You know... install packages."
  (interactive)
  (dolist (p user-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(za/install-packages)

;;----------------------------------------------------------------------------
;;-- bootstrap.macros
;;----------------------------------------------------------------------------

(defmacro after (mode &rest body)
  "After MODE loads, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;----------------------------------------------------------------------------
;;-- init.clojure
;;----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))

(after 'clojure-mode
  (define-clojure-indent
    ;; midje
    (fact 'defun)
    (facts 'defun)

    ;; core.logic
    (run* 'defun)
    (run 'defun)
    (fresh 'defun)
    (defne 'defun)
    (project 'defun)
    (matche 'defun)
    (conde 'defun)

    ;; core.match
    (match 'defun)

    ;; core.async
    (go 'defun)
    (go-loop 'defun)
    (alt! 'defun)

    ;; core.async helpers
    (pipeline 'defun)
    (while-open 'defun)
    (when-recv 'defun)
    (if-recv 'defun)

    ;; simple-check
    (for-all 'defun)

    ;; random stuff
    (test 'defun)
    (go-test-all 'defun)))

;;-- init.clojure.cider

(defun cider-send-dwim ()
  "Send the appropriate forms to the REPL to be evaluated."
  (interactive)
  (let ((expr (cider-last-expression)))
    (pop-to-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert expr)
    (cider-return)
    (other-window 1)))

(defun cider-send-reset (refresh?)
  "Send the form '(do (in-ns 'user) (reset)) to the REPL. Given a
prefix, send the form '(do (in-ns 'user) (refresh))."
  (interactive "P")
  (let ((form (if refresh?
                  "(do (in-ns 'user) (refresh))"
                "(do (in-ns 'user) (reset))")))
    (pop-to-buffer (cider-find-or-create-repl-buffer))
    (insert form)
    (cider-return)
    (other-window 1)))

(after 'cider
  (define-key cider-mode-map (kbd "C-c C-c") 'cider-send-dwim)
  (define-key cider-mode-map (kbd "C-c C-r") 'cider-send-reset)

  (add-hook 'cider-mode-hook
            'cider-turn-on-eldoc-mode)

  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode-enable)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (font-lock-mode nil)
              (clojure-mode-font-lock-setup)
              (font-lock-mode t))))

;;----------------------------------------------------------------------------
;;-- init.paredit
;;----------------------------------------------------------------------------

;; don't insert a space before delimiters
(after 'paredit
  (add-hook 'paredit-mode-hook
            (lambda ()
              (add-to-list (make-local-variable
                            'paredit-space-for-delimiter-predicates)
                           (lambda (_ _) nil)))))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(after 'clojure-mode     (add-hook 'clojure-mode-hook 'paredit-mode))
(after 'nrepl            (add-hook 'nrepl-mode-hook 'paredit-mode))
(after 'scheme-mode      (add-hook 'scheme-mode-hook 'paredit-mode))
(after 'common-lisp-mode (add-hook 'lisp-mode-hook 'paredit-mode))

;; Setup Paredit mode
(require 'paredit)
(require 'clojure-mode)
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

;; Add additional clojure file associations
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))
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
