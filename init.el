;;; -*- lexical-binding: t -*-

;; ------------------------------------------------------------

(setq gc-cons-threshold (* 10 1024 1024))
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;(setq debug-on-error t)

;; ------------------------------------------------------------

;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'visual)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (display-line-numbers-mode 1)
              (electric-pair-local-mode 1)))

(defvar init/default-font "TamzenForPowerline-11:antialias=none")
(add-to-list 'default-frame-alist `(font . ,init/default-font))

;; ------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-readable-p custom-file) (load custom-file))

(defvar init/lisp-dir (concat user-emacs-directory "lisp/"))
(if (file-readable-p init/lisp-dir)
    (let ((default-directory init/lisp-dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path)))

;; ------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
;(setq use-package-compute-statistics t)

;; ------------------------------------------------------------

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night)

  (defun init/get-theme-color (name)
    (let ((theme-colors (alist-get 'night color-theme-sanityinc-tomorrow-colors)))
      (alist-get name theme-colors)))

  (set-face-attribute 'cursor nil :background (init/get-theme-color 'red)))

;; ------------------------------------------------------------

(defvar init/win-key-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "m") 'windmove-left)
    (define-key map (kbd "i") 'windmove-right)
    (define-key map (kbd "n") 'windmove-down)
    (define-key map (kbd "e") 'windmove-up)
    (define-key map (kbd "C-m") 'windmove-left)
    (define-key map (kbd "C-i") 'windmove-right)
    (define-key map (kbd "C-n") 'windmove-down)
    (define-key map (kbd "C-e") 'windmove-up)

    (define-key map (kbd "o") 'previous-window-any-frame)
    (define-key map (kbd "t") 'next-window-any-frame)
    (define-key map (kbd "C-o") 'previous-window-any-frame)
    (define-key map (kbd "C-t") 'next-window-any-frame)

    (define-key map (kbd "s") 'split-window-vertically)
    (define-key map (kbd "v") 'split-window-horizontally)
    (define-key map (kbd "q") 'delete-window)
    (define-key map (kbd "d") 'delete-other-windows)
    (define-key map (kbd "C-s") 'split-window-vertically)
    (define-key map (kbd "C-v") 'split-window-horizontally)
    (define-key map (kbd "C-q") 'delete-window)
    (define-key map (kbd "C-d") 'delete-other-windows)

    (define-key map (kbd "M") 'shrink-window-horizontally)
    (define-key map (kbd "I") 'enlarge-window-horizontally)
    (define-key map (kbd "N") 'shrink-window)
    (define-key map (kbd "E") 'enlarge-window)
    (define-key map (kbd "C-S-M") 'shrink-window-horizontally)
    (define-key map (kbd "C-S-I") 'enlarge-window-horizontally)
    (define-key map (kbd "C-S-N") 'shrink-window)
    (define-key map (kbd "C-S-E") 'enlarge-window)
    (define-key map (kbd "=") 'balance-windows)
    (define-key map (kbd "C-=") 'balance-windows)

    (define-key map (kbd "k") 'vterm)
    (define-key map (kbd "K") 'vterm-other-window)
    (define-key map (kbd "C-k") 'vterm)
    (define-key map (kbd "C-S-K") 'vterm-other-window)
    (define-key map (kbd "h") 'eshell)
    ;(define-key map (kbd "C-h") 'eshell) ;; occupied by which-key paging

    (define-key map (kbd "f") 'ido-find-file-other-window)
    (define-key map (kbd "C-f") 'ido-find-file-other-window)
    (define-key map (kbd "F") 'ido-find-file-other-frame)
    (define-key map (kbd "C-S-F") 'ido-find-file-other-frame)

    (define-key map (kbd "g") 'ido-switch-buffer-other-window)
    ;(define-key map (kbd "C-g") 'ido-switch-buffer-other-window) ;; occupied by cmd cancelation
    (define-key map (kbd "G") 'ido-switch-buffer-other-frame)
    (define-key map (kbd "C-S-G") 'ido-switch-buffer-other-frame)

    (define-key map (kbd "C-c") 'delete-frame)

    map))

(defvar init/buf-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'ido-switch-buffer)
    (define-key map (kbd "t") 'ido-switch-buffer-other-window)
    (define-key map (kbd "T") 'ido-switch-buffer-other-frame)
    (define-key map (kbd "k") 'ido-kill-buffer)
    map))

(defvar init/file-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'ido-find-file)
    (define-key map (kbd "t") 'ido-find-file-other-window)
    (define-key map (kbd "T") 'ido-find-file-other-frame)
    (define-key map (kbd "s") 'save-buffer)
    (define-key map (kbd "r") 'revert-buffer)
    (define-key map (kbd "d") 'ido-dired)
    map))

(defun init/bind-comma-keys (&optional keymap)
  (let ((map (or keymap (current-local-map))))
    (define-key map (kbd ", h") 'help-command)
    (define-key map (kbd ", x") ctl-x-map)
    (define-key map (kbd ", t") init/win-key-map)
    (define-key map (kbd ", g") init/buf-key-map)
    (define-key map (kbd ", f") init/file-key-map)

    (define-key map (kbd "-") (kbd "M--"))
    (define-key map (kbd "0") (kbd "M-0"))
    (define-key map (kbd "1") (kbd "M-1"))
    (define-key map (kbd "2") (kbd "M-2"))
    (define-key map (kbd "3") (kbd "M-3"))
    (define-key map (kbd "4") (kbd "M-4"))
    (define-key map (kbd "5") (kbd "M-5"))
    (define-key map (kbd "6") (kbd "M-6"))
    (define-key map (kbd "7") (kbd "M-7"))
    (define-key map (kbd "8") (kbd "M-8"))
    (define-key map (kbd "9") (kbd "M-9"))
    
    map))

(advice-add 'display-startup-screen
            :after
            #'(lambda (&rest args)
                (ignore args)
                (with-current-buffer "*GNU Emacs*"
                  (init/bind-comma-keys))))

(defun init/deactivate-mark ()
  (interactive)
  (deactivate-mark))

(defun init/ensure-mark-active ()
  (interactive)
  (unless (use-region-p) (push-mark (point) nil t)))

(defun init/open-lines-below (count)
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun init/open-lines-above (count)
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun init/select-lines (count)
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (push-mark (point) nil t))
  (forward-line count))

(defun init/kill-selection (count)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (progn
      (push-mark (point) nil t)
      (forward-char count)
      (kill-region (region-beginning) (region-end))
      (deactivate-mark))))

(defun init/kill-ring-save-selection (count)
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (push-mark (point) nil t)
      (forward-char count)
      (kill-ring-save (region-beginning) (region-end))
      (exchange-point-and-mark)
      (deactivate-mark))))

(defun init/goto-line (&optional line)
  (interactive "p")
  (push-mark (point))
  (goto-char (point-min))
  (when line
    (forward-line (- line 1))))

(defun init/goto-prev-mark ()
  (interactive)
  (set-mark-command t))

(defun init/goto-next-mark ()
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

;; to be wrapped by mc--cache-input-function, so that read-string stay untouched
(defun init/mc-read-string (&rest args)
  (apply #'read-string args))

(defun init/wrap-with (start end str)
  (interactive
   (if buffer-read-only
       (user-error "Buffer is read-only: %S" (current-buffer))
     (let ((input-str (init/mc-read-string "Wrap with: " nil nil nil t)))
       (list (region-beginning) (region-end) input-str))))
  (when (use-region-p)
    (let* ((str-len (length str))
           (mid-idx (/ str-len 2))
           (left-end (if (<= mid-idx 0) str-len mid-idx))
           (right-start mid-idx)
           (left (substring str 0 left-end))
           (right (substring str right-start str-len))
           (new-end end)
           (old-point (point)))
      (save-mark-and-excursion
        (goto-char end)
        ;; mark the end
        (set-mark (point))
        ;; also moves mark (the end) to the right
        (insert-before-markers-and-inherit right)
        (goto-char start)
        ;; also moves point & mark (the end) to the right
        (insert-before-markers-and-inherit left)
        ;; save the new end, to be used to delimit the new region after all this mangling
        (setq new-end (mark))
        ;; the old point was shifted the same amount as the start
        (setq old-point (+ old-point (- (point) start))))
      ;; to make exchange-point-and-mark work properly and move point to nearest location
      (push-mark new-end nil nil)
      (goto-char start)
      (if (< (abs (- old-point new-end)) (abs (- old-point start)))
          (exchange-point-and-mark)))))

(defun init/unwrap (start end count)
  (interactive "*r\np")
  (when (and (use-region-p) (>= (- end start) (* count 2)))
    (save-mark-and-excursion
      (delete-region (- end count) end)
      (delete-region start (+ start count)))))

;; ------------------------------------------------------------

(use-package multistate
  :init
  (multistate-define-state
   'emacs
   :lighter "emacs"
   :cursor 'bar)

  (multistate-define-state
   'cmd
   :default t
   :lighter "cmd"
   :cursor 'box
   :parent 'multistate-emacs-state-map)

  ;; remove "reference to free variable" warnings
  (defvar multistate-emacs-state-map)
  (defvar multistate-cmd-state-map)

  (let ((map multistate-emacs-state-map))
    (define-key map (kbd "C-t") init/win-key-map))

  (let ((map multistate-cmd-state-map))
    (define-key map (kbd ", x") ctl-x-map)
    (define-key map (kbd ", t") init/win-key-map)
    (define-key map (kbd ", g") init/buf-key-map)
    (define-key map (kbd ", f") init/file-key-map))

  (setq multistate-lighter-format "%s")

  (multistate-global-mode 1)

  :bind
  (:map
   multistate-emacs-state-map
   ("C-z" . multistate-cmd-state)

   :map multistate-cmd-state-map
   ("C-z" . multistate-emacs-state)
   ("z" . multistate-emacs-state)

   (", h" . help-command)

   (":" . execute-extended-command)

   ("." . repeat)

   ("m" . backward-char)
   ("i" . forward-char)
   ("n" . next-line)
   ("e" . previous-line)

   ("M" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'beginning-of-line)))
   ("I" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'end-of-line)))
   ("N" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'next-line)))
   ("E" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'previous-line)))

   ("w" . forward-word)
   ("b" . backward-word)

   ("W" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'forward-word)))
   ("B" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'backward-word)))

   ("s" . forward-sexp)
   ("r" . backward-sexp)

   ("S" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'forward-sexp)))
   ("R" . (lambda ()
            (interactive)
            (init/ensure-mark-active)
            (call-interactively 'backward-sexp)))

   ("g m" . back-to-indentation)
   ("g M" . move-beginning-of-line)
   ("g i" . move-end-of-line)
   ("g n" . forward-paragraph)
   ("g e" . backward-paragraph)

   ("g w" . forward-sentence)
   ("g b" . backward-sentence)

   ("g u" . backward-up-list)
   ("g U" . up-list)
   ("g d" . down-list)

   ("g k" . init/goto-prev-mark)
   ("g h" . init/goto-next-mark)
   ("g K" . pop-global-mark)

   ("g g" . init/goto-line)
   ("g G" . end-of-buffer)

   ("G" . end-of-buffer)

   (";" . init/deactivate-mark)
   ("M-;" . exchange-point-and-mark)
   ("x" . init/select-lines)

   ("'" . init/wrap-with)
   ("\"" . init/unwrap)

   ("y" . init/kill-ring-save-selection)
   ("d" . init/kill-selection)
   ("p" . yank)
   ("u" . undo)

   ("Q" . kmacro-start-macro-or-insert-counter)
   ("q" . kmacro-end-or-call-macro)

   ("j" . delete-indentation)
   ("J" . (lambda ()
            (interactive "*")
            (delete-indentation t)))

   ("a" . (lambda ()
            (interactive "*")
            (call-interactively 'forward-char)
            (multistate-emacs-state)))
   ("A" . (lambda ()
            (interactive "*")
            (call-interactively 'move-end-of-line)
            (multistate-emacs-state)))
   ("Z" . (lambda ()
            (interactive "*")
            (back-to-indentation)
            (multistate-emacs-state)))
   ("o" . (lambda ()
            (interactive "*")
            (call-interactively 'init/open-lines-below)
            (multistate-emacs-state)))
   ("O" . (lambda ()
            (interactive "*")
            (call-interactively 'init/open-lines-above)
            (multistate-emacs-state)))
   ("c" . (lambda ()
            (interactive "*")
            (call-interactively 'init/kill-selection)
            (multistate-emacs-state)))

   ("-" . "\M--")
   ("0" . "\M-0")
   ("1" . "\M-1")
   ("2" . "\M-2")
   ("3" . "\M-3")
   ("4" . "\M-4")
   ("5" . "\M-5")
   ("6" . "\M-6")
   ("7" . "\M-7")
   ("8" . "\M-8")
   ("9" . "\M-9")))

;; ------------------------------------------------------------

(use-package ido
  :ensure nil ;; built-in package
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  :bind
  (:map
   ido-common-completion-map
   ("M-n" . ido-next-match)
   ("M-p" . ido-prev-match)))

(use-package hippie-exp
  :ensure nil ;; build-in package
  :defer t
  :after (:all multistate)
  :config

  (setq hippie-expand-try-functions-list
        '(try-expand-all-abbrevs
          try-expand-list
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-line
          try-complete-file-name-partially
          try-complete-file-name
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (defun init/hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'init/hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand))
          (warning-suppress-types (cons '(undo discard-info) warning-suppress-types)))
      (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'init/hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defun init/ido-hippie-expand-with (hippie-expand-function)
    "Offer ido-based completion using the specified hippie-expand function."
    (let* ((options (init/hippie-expand-completions hippie-expand-function))
           (sorted-options (cl-sort options
                                    #'(lambda (s1 s2)
                                        (if (= (length s1) (length s2))
                                            (string-lessp s1 s2)
                                          (< (length s1) (length s2))))))
           (selection (and sorted-options
                           (ido-completing-read "Completions: " sorted-options nil nil he-search-string))))
      (if selection
          (he-substitute-string selection t)
        (message "No expansion found"))))

  (defun init/ido-hippie-expand ()
    "Offer ido-based completion for the word at point."
    (interactive)
    (init/ido-hippie-expand-with 'hippie-expand))

  :bind
  (:map
   multistate-emacs-state-map
   ("M-n" . init/ido-hippie-expand)))

;; ------------------------------------------------------------

(use-package winner
  :ensure nil ;; built-in package
  :defer nil  ;; suppress implicit :defer by :bind
  :config
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)

  :bind
  (:map
   init/win-key-map
   ("u" . winner-undo)
   ("C-u" . winner-undo)
   ("r" . winner-redo)
   ("C-r" . winner-redo)))

;; ------------------------------------------------------------

(use-package savehist
  :ensure nil ;; built-in package
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'last-kbd-macro))

;; ------------------------------------------------------------

(use-package dired
  :ensure nil  ;; built-in package
  :config
  (setq dired-dwim-target t))

;; ------------------------------------------------------------

(use-package which-func
  :ensure nil  ;; built-in package
  :config
  (which-function-mode 1))

;; ------------------------------------------------------------

(use-package which-key
  :config
  (which-key-mode 1))

;; ------------------------------------------------------------

(use-package expand-region
  :defer t
  :after (:all multistate)
  :bind
  (:map
   multistate-cmd-state-map
   ("v" . er/expand-region)
   ("V" . set-mark-command)))

;; ------------------------------------------------------------

(use-package multiple-cursors
  :defer t
  :after (:all multistate)

  :init
  (multistate-define-state
   'mc
   :lighter "mc"
   :cursor 'box
   :parent 'multistate-cmd-state-map)

  ;; to remove "reference to free variable" warnings
  (defvar multistate-mc-state-map)

  :bind
  (:map
   multistate-cmd-state-map
   (", c l" . mc/edit-lines)
   (", c a" . mc/mark-all-like-this)
   (", c s" . mc/mark-all-in-region)
   (", c r" . mc/mark-all-in-region-regexp)
   (", c m" . multistate-mc-state)

   :map
   multistate-mc-state-map
   ("n" . mc/mark-next-like-this)
   ("E" . mc/unmark-next-like-this)
   ("M-n" . mc/skip-to-next-like-this)
   ("e" . mc/mark-previous-like-this)
   ("N" . mc/unmark-previous-like-this)
   ("M-e" . mc/skip-to-previous-like-this)
   ("m" . multistate-cmd-state))

  :config
  (defun init/cache-mc-read-string ()
    ;; only available after multiple-cursors is loaded
    (mc--cache-input-function init/mc-read-string car))

  :hook
  ; won't work in :config, don't know why
  (after-init . init/cache-mc-read-string))

;; ------------------------------------------------------------

(use-package phi-search
  :defer t
  :after (:all multistate)

  :bind
  (:map
   multistate-emacs-state-map
   ("C-s" . phi-search)
   ("C-r" . phi-search-backward)

   :map
   multistate-cmd-state-map
   ("f" . phi-search)
   ("F" . phi-search-backward)))

;; ------------------------------------------------------------

(use-package sml-modeline
  :config
  (sml-modeline-mode 1))

;; ------------------------------------------------------------

(use-package imenu-anywhere
  :defer t
  :after (:all multistate ido)
  :bind
  (:map
   multistate-cmd-state-map
   ("g t" . ido-imenu-anywhere)))

;; ------------------------------------------------------------

;; emacs wants this in the top level, or there'll be
;; "'make-variable-buffer-local' not called at toplevel" warnings
;; when byte-compiling
(defvar-local init/citre/prev-dln-mode 0)

(use-package citre
  :defer t
  :after (:all multistate color-theme-sanityinc-tomorrow)

  :init
  (require 'citre-config)
  (autoload 'citre-peek-restore "citre" "Should have been autoloaded by citre...." t nil)

  (multistate-define-state
   'citre-peek
   :lighter "citre-peek"
   :cursor 'box
   :parent 'multistate-cmd-state-map)

  ;; remove "reference to free variable" warnings
  (defvar multistate-citre-peek-state-map)

  :bind
  (:map
   multistate-cmd-state-map
   ("g p" . citre-peek)
   ("g P" . citre-ace-peek)
   ("g r" . citre-peek-restore)

   :map
   multistate-citre-peek-state-map
   ("n" . citre-peek-next-line)
   ("e" . citre-peek-prev-line)
   ("N" . citre-peek-next-tag)
   ("E" . citre-peek-prev-tag)
   ("i" . citre-peek-chain-forward)
   ("m" . citre-peek-chain-backward)
   ("I" . citre-peek-next-branch)
   ("M" . citre-peek-prev-branch)
   ("l p" . citre-peek-through)
   ("l r" . citre-peek-through-reference)
   ("l d" . citre-peek-delete-branch)
   ("l D" . citre-peek-delete-branches)
   ("l f" . citre-peek-make-current-tag-first)
   ("l j" . citre-peek-jump)
   ("q" . citre-peek-abort)
   ("C-l" . recenter-top-bottom))

  :config
  (setq citre-peek-file-content-height 22)
  (setq citre-peek-tag-list-height 3)

  (set-face-attribute 'citre-peek-border-face nil
                      :background (init/get-theme-color 'blue))
  (set-face-attribute 'citre-peek-ace-str-face nil
                      :foreground (init/get-theme-color 'background)
                      :background (init/get-theme-color 'blue))

  (add-hook 'citre-peek--mode-hook
            #'(lambda ()
                (if citre-peek--mode
                    (progn
                      ;; display-line-numbers-mode tears the border apart,
                      ;; so disable it when the peek window is shown
                      (setq init/citre/prev-dln-mode
                            (if display-line-numbers-mode 1 -1))
                      (display-line-numbers-mode -1)
                      (multistate-citre-peek-state))
                  (progn
                    (multistate-cmd-state)
                    (display-line-numbers-mode init/citre/prev-dln-mode))))))

;; ------------------------------------------------------------

(use-package eglot
  :defer t)

;; ------------------------------------------------------------

(use-package vc-fossil
  :defer t
  :init
  (add-to-list 'vc-handled-backends 'Fossil t))

;; ------------------------------------------------------------

(use-package vterm
  :defer t
  :after (:all multistate)

  :init
  (multistate-define-state
   'vterm
   :lighter "vterm"
   :cursor 'bar
   :parent 'multistate-emacs-state-map)

  (multistate-define-state
   'vterm-copy
   :lighter "vterm-copy"
   :cursor 'box
   :parent 'multistate-cmd-state-map)

  ;; remove "reference to free variable" warnings
  (defvar multistate-vterm-state-map)
  (defvar multistate-vterm-copy-state-map)

  :bind
  (:map
   multistate-vterm-state-map
   ("C-z" . vterm-copy-mode)
   ("C-c C-z" . vterm--self-insert)
   ("C-c C-t" . vterm--self-insert)
   ("M-n" . vterm--self-insert)
   ("C-s" . vterm--self-insert)
   ("C-r" . vterm--self-insert)

   :map
   multistate-vterm-copy-state-map
   ("z" . vterm-copy-mode)
   ("C-z" . vterm-copy-mode))

  :config
  (add-hook 'vterm-mode-hook
            #'(lambda ()
                (multistate-vterm-state)))

  ;; remove "reference to free variable" warnings
  (defvar vterm-copy-mode)
  (add-hook 'vterm-copy-mode-hook
            #'(lambda ()
                (if vterm-copy-mode
                  (multistate-vterm-copy-state)
                  (multistate-vterm-state)))))

;; ------------------------------------------------------------

(use-package ement
  :defer t
  :after (:all multistate)
  :config
  ;; remove "reference to free variable" warnings
  (defvar ement-room-list-mode-map)
  (defvar ement-room-mode-map)

  (add-hook 'ement-room-list-mode-hook
            #'(lambda ()
                (multistate-emacs-state)
                (let ((map ement-room-list-mode-map))
                  (define-key map (kbd "m") 'left-char)
                  (define-key map (kbd "i") 'right-char)
                  (define-key map (kbd "n") 'magit-section-forward)
                  (define-key map (kbd "e") 'magit-section-backward)
                  (define-key map (kbd "p") 'magit-section-backward)
                  (define-key map (kbd "M-g M-n") 'ement-notify-switch-to-notifications-buffer)
                  (define-key map (kbd "M-g M-m") 'ement-notify-switch-to-mentions-buffer))))

  (add-hook 'ement-room-mode-hook
            #'(lambda ()
                (multistate-emacs-state)
                (let ((map ement-room-mode-map))
                  (define-key map (kbd "m") 'left-char)
                  (define-key map (kbd "i") 'right-char)
                  (define-key map (kbd "n") 'ement-room-goto-next)
                  (define-key map (kbd "e") 'ement-room-goto-prev)
                  (define-key map (kbd "p") 'ement-room-goto-prev)))))

;; ------------------------------------------------------------

(use-package markdown-mode
  :defer t)

;; ------------------------------------------------------------

(use-package cc-mode
  :ensure nil  ;; built-in package
  :defer t
  :config
  (setq c-default-style "linux")

  (defun init/setup-cc-mode ()
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4))

  :hook
  (c-mode-common . init/setup-cc-mode))

;; ------------------------------------------------------------

(use-package kotlin-mode
  :defer t)

;; ------------------------------------------------------------

(use-package zig-mode
  :defer t)

;; ------------------------------------------------------------

(use-package lilypond-mode
  :ensure nil  ;; local package
  :defer t
  :init
  (load-library "lilypond-init"))

;; ------------------------------------------------------------

(use-package janet-mode
  :ensure nil  ;; local package
  :defer t
  :init
  (load-library "janet-mode-autoloads"))

;; ------------------------------------------------------------

(defvar init/local-env-file (concat user-emacs-directory "local-env.el"))
(if (file-readable-p init/local-env-file) (load init/local-env-file))
