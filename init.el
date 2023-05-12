;;; -*- lexical-binding: t -*-

;; ------------------------------------------------------------

(setq gc-cons-threshold (* 10 1024 1024))
(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'last-kbd-macro)

;(setq debug-on-error t)

;; ------------------------------------------------------------

;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

(defvar init/default-font "TamzenForPowerline-11:antialias=none")
(add-to-list 'default-frame-alist `(font . ,init/default-font))

(setq-default cursor-type 'bar)

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

    (define-key map (kbd "k") 'vterm)
    (define-key map (kbd "K") 'vterm-other-window)
    (define-key map (kbd "C-k") 'vterm)
    (define-key map (kbd "C-S-K") 'vterm-other-window)
    (define-key map (kbd "h") 'eshell)
    (define-key map (kbd "C-h") 'eshell)

    (define-key map (kbd "f") 'ido-find-file-other-window)
    (define-key map (kbd "C-f") 'ido-find-file-other-window)
    (define-key map (kbd "F") 'ido-find-file-other-frame)
    (define-key map (kbd "C-S-F") 'ido-find-file-other-frame)

    (define-key map (kbd "g") 'ido-switch-buffer-other-window)
    (define-key map (kbd "C-g") 'ido-switch-buffer-other-window)
    (define-key map (kbd "G") 'ido-switch-buffer-other-frame)
    (define-key map (kbd "C-S-G") 'ido-switch-buffer-other-frame)

    (define-key map (kbd "C-c") 'delete-frame)

    map))

(global-unset-key (kbd "C-t")) ;; Was transpose-chars; Use C-t for window management instead
(define-key (current-global-map) (kbd "C-t") init/win-key-map)

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

(define-key (current-global-map) (kbd "M-n") 'hippie-expand)

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

(use-package ryo-modal
  :bind ("C-z" . ryo-modal-mode)
  :hook (after-init . init/ryo-modal-setup)
  :defer t
  ;; load after the theme, so that ryo-modal knows about the default cursor color
  :after (:all color-theme-sanityinc-tomorrow)
  :config

  (add-hook 'text-mode-hook #'(lambda () (ryo-modal-mode 1)))
  (add-hook 'prog-mode-hook #'(lambda () (ryo-modal-mode 1)))

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

  ;; to be wrapped by mc--cache-input-function, so that read-string stay untouched
  (defun init/mc-read-string (&rest args)
    (apply #'read-string args))

  (defun init/wrap-with (start end str)
    (interactive
     (let ((input-str (init/mc-read-string "Wrap with: " nil nil nil t)))
       (list (region-beginning) (region-end) input-str)))
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
    (interactive "r\np")
    (when (and (use-region-p) (>= (- end start) (* count 2)))
      (save-mark-and-excursion
        (delete-region (- end count) end)
        (delete-region start (+ start count)))))

  ;; ------------------------------------------------------------

  (defun init/ryo-modal-setup ()
    (setq ryo-modal-cursor-color (init/get-theme-color 'red))

    (ryo-modal-keys
     ("z" ryo-modal-mode)

     (","
      (("h" help-command)))

     (":" execute-extended-command)

     ("." ryo-modal-repeat)

     ("m" backward-char)
     ("i" forward-char)
     ("n" next-line)
     ("e" previous-line)

     ("M" beginning-of-line :first '(init/ensure-mark-active))
     ("I" end-of-line :first '(init/ensure-mark-active))
     ("N" next-line :first '(init/ensure-mark-active))
     ("E" previous-line :first '(init/ensure-mark-active))

     ("w" forward-word)
     ("b" backward-word)

     ("W" forward-word :first '(init/ensure-mark-active))
     ("B" backward-word :first '(init/ensure-mark-active))

     ("s" forward-sexp)
     ("r" backward-sexp)

     ("S" forward-sexp :first '(init/ensure-mark-active))
     ("R" backward-sexp :first '(init/ensure-mark-active))

     ("g"
      (("m" back-to-indentation)
       ("M" move-beginning-of-line)
       ("i" move-end-of-line)
       ("n" forward-paragraph)
       ("e" backward-paragraph)

       ("w" forward-sentence)
       ("b" backward-sentence)

       ("u" backward-up-list)
       ("U" up-list)
       ("d" down-list)

       ("g" init/goto-line)
       ("G" end-of-buffer)))
     ("G" end-of-buffer)

     (";" init/deactivate-mark)
     ("M-;" exchange-point-and-mark)
     ("x" init/select-lines)

     ("'" init/wrap-with)
     ("\"" init/unwrap)

     ("y" init/kill-ring-save-selection)
     ("d" init/kill-selection)
     ("p" yank)
     ("u" undo)

     ("Q" kmacro-start-macro-or-insert-counter)
     ("q" kmacro-end-or-call-macro)

     ("a" forward-char :exit t)
     ("A" move-end-of-line :exit t)
     ("Z" back-to-indentation :exit t)
     ("o" init/open-lines-below :exit t)
     ("O" init/open-lines-above :exit t)
     ("c" init/kill-selection :exit t))

    (define-key ryo-modal-mode-map (kbd ", x") ctl-x-map)
    (define-key ryo-modal-mode-map (kbd ", t") init/win-key-map)
    (define-key ryo-modal-mode-map (kbd ", g") init/buf-key-map)
    (define-key ryo-modal-mode-map (kbd ", f") init/file-key-map)

    (ryo-modal-keys
     (:norepeat t)
     ("-" "M--")
     ("0" "M-0")
     ("1" "M-1")
     ("2" "M-2")
     ("3" "M-3")
     ("4" "M-4")
     ("5" "M-5")
     ("6" "M-6")
     ("7" "M-7")
     ("8" "M-8")
     ("9" "M-9"))))

;; ------------------------------------------------------------

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

;; ------------------------------------------------------------

(use-package which-func
  :config
  (which-function-mode 1))

;; ------------------------------------------------------------

(use-package dired
  :ensure nil    ;; built-in package
  :hook (dired-mode . init/bind-comma-keys))

;; ------------------------------------------------------------

(use-package help-mode
  :ensure nil    ;; built-in package
  :hook (help-mode . init/bind-comma-keys))

;; ------------------------------------------------------------

(use-package which-key
  :defer t
  :config
  (which-key-mode))

;; ------------------------------------------------------------

(use-package expand-region
  :defer t
  :after (:all ryo-modal)
  :init
  (ryo-modal-keys
   ("v" er/expand-region)
   ("V" set-mark-command)))

;; ------------------------------------------------------------

(defvar init/mc-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'mc/mark-next-like-this)
    (define-key map (kbd "E") 'mc/unmark-next-like-this)
    (define-key map (kbd "M-n") 'mc/skip-to-next-like-this)
    (define-key map (kbd "e") 'mc/mark-previous-like-this)
    (define-key map (kbd "N") 'mc/unmark-previous-like-this)
    (define-key map (kbd "M-e") 'mc/skip-to-previous-like-this)
    (define-key map (kbd "m") 'init/mc-mode)
    map))

;; caused "Waning: 'make-variable-buffer-local' not called at toplevel" while
;; compiling if put inside a use-package call
(define-minor-mode init/mc-mode
  nil
  :init-value nil
  :lighter " mc"
  :keymap init/mc-key-map)

(use-package multiple-cursors
  :defer t
  :after (:all ryo-modal)
  :init
  (ryo-modal-keys
   (","
    (("c"
      (("l" mc/edit-lines)
       ("a" mc/mark-all-like-this)
       ("s" mc/mark-all-in-region)
       ("r" mc/mark-all-in-region-regexp)
       ("m" init/mc-mode))))))

  :config
  (defun init/cache-mc-read-string ()
    ;; only available after multiple-cursors is loaded
    (mc--cache-input-function init/mc-read-string))

  :hook
  ; won't work in :config, don't know why
  (after-init . init/cache-mc-read-string))

;; ------------------------------------------------------------

(use-package phi-search
  :defer t
  :after (:all ryo-modal)
  :init
  (define-key (current-global-map) (kbd "C-s") 'phi-search)
  (define-key (current-global-map) (kbd "C-r") 'phi-search-backward)

  (ryo-modal-keys
   ("f" phi-search)
   ("M-f" phi-search-backward)))

;; ------------------------------------------------------------

(use-package sml-modeline
  :config
  (sml-modeline-mode 1))

;; ------------------------------------------------------------

(use-package imenu-anywhere
  :defer t
  :after (:all ryo-modal ido)
  :init
  (ryo-modal-keys
   ("g"
    (("t" ido-imenu-anywhere)))))

;; ------------------------------------------------------------

(use-package citre
  :defer t
  :after (:all ryo-modal color-theme-sanityinc-tomorrow)
  :init
  (require 'citre-config)
  (autoload 'citre-peek-restore "citre" "Should have been autoloaded by citre...." t nil)
  (ryo-modal-keys
   ("g"
    (("p" citre-peek)
     ("P" citre-ace-peek)
     ("r" citre-peek-restore))))

  :config
  (let ((map citre-peek-keymap))
    (define-key map (kbd "n") 'citre-peek-next-line)
    (define-key map (kbd "e") 'citre-peek-prev-line)
    (define-key map (kbd "N") 'citre-peek-next-tag)
    (define-key map (kbd "E") 'citre-peek-prev-tag)
    (define-key map (kbd "i") 'citre-peek-chain-forward)
    (define-key map (kbd "m") 'citre-peek-chain-backward)
    (define-key map (kbd "I") 'citre-peek-next-branch)
    (define-key map (kbd "M") 'citre-peek-prev-branch)

    (define-key map (kbd "l p") 'citre-peek-through)
    (define-key map (kbd "l r") 'citre-peek-through-reference)
    (define-key map (kbd "l d") 'citre-peek-delete-branch)
    (define-key map (kbd "l D") 'citre-peek-delete-branches)
    (define-key map (kbd "l f") 'citre-peek-make-current-tag-first)
    (define-key map (kbd "l j") 'citre-peek-jump))

  (setq citre-peek-file-content-height 22)
  (setq citre-peek-tag-list-height 3)
  ;; display-line-numbers-mode tears the border apart when running without this
  (setq citre-peek-fill-fringe nil)

  (set-face-attribute 'citre-peek-border-face nil
                      :background (init/get-theme-color 'blue))
  (set-face-attribute 'citre-peek-ace-str-face nil
                      :foreground (init/get-theme-color 'background)
                      :background (init/get-theme-color 'blue)))

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
  :config
  (add-hook 'vterm-mode-hook #'(lambda () (display-line-numbers-mode -1)))

  (defvar init/vterm-copy-local-map (make-sparse-keymap))
  (add-hook 'vterm-copy-mode-hook
            #'(lambda ()
                (when (and (boundp 'vterm-copy-mode) vterm-copy-mode)
                  (when (not (current-local-map))
                    (use-local-map init/vterm-copy-local-map))
                  (init/bind-comma-keys)
                  (let ((map (current-local-map)))
                    (define-key map (kbd ", c n") 'vterm-next-prompt)
                    (define-key map (kbd ", c p") 'vterm-previous-prompt)
                    (define-key map (kbd ", c r") 'vterm-reset-cursor-point)
                    (define-key map (kbd ", c t") 'vterm-copy-mode))))))

;; ------------------------------------------------------------

(use-package markdown-mode
  :defer t)

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
  (load-library "lilypond-init")
  (add-hook 'LilyPond-mode-hook #'(lambda () (ryo-modal-mode 1))))

;; ------------------------------------------------------------

(use-package janet-mode
  :ensure nil  ;; local package
  :defer t
  :init
  (load-library "janet-mode-autoloads"))

;; ------------------------------------------------------------

(defvar init/local-env-file (concat user-emacs-directory "local-env.el"))
(if (file-readable-p init/local-env-file) (load init/local-env-file))
