;; ------------------------------------------------------------

(setq gc-cons-threshold (* 10 1024 1024))
(setq make-backup-files nil)

;; ------------------------------------------------------------

;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

;; ------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file) (load custom-file))

;; ------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ------------------------------------------------------------

(use-package ryo-modal
  :bind ("C-z" . ryo-modal-mode)
  :hook (after-init . init/ryo-modal-setup)
  :config

  (add-hook 'prog-mode-hook #'(lambda () (ryo-modal-mode 1)))

  (defun init/deactivate-mark ()
    (interactive)
    (deactivate-mark))

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
    (unless (use-region-p) (set-mark (point)))
    (forward-line count))

  (defun init/select-line-before-point ()
    (interactive)
    (set-mark (point))
    (beginning-of-line))

  (defun init/select-line-after-point ()
    (interactive)
    (set-mark (point))
    (end-of-line))

  (defun init/select-to-next-line (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (next-line count))

  (defun init/select-to-previous-line (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (previous-line count))

  (defun init/select-to-forward-word (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (forward-word count))

  (defun init/select-to-backward-word (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (backward-word count))

  (defun init/select-to-forward-sexp (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (forward-sexp count))

  (defun init/select-to-backward-sexp (count)
    (interactive "p")
    (unless (use-region-p) (set-mark (point)))
    (backward-sexp count))

  (defun init/kill-selection (count)
    (interactive "p")
    (if (use-region-p)
	(kill-region (region-beginning) (region-end))
      (progn
	(set-mark (point))
	(forward-char count)
	(kill-region (region-beginning) (region-end))
	(deactivate-mark))))

  (defun init/kill-ring-save-selection (count)
    (interactive "p")
    (if (use-region-p)
	(kill-ring-save (region-beginning) (region-end))
      (progn
	(set-mark (point))
	(forward-char count)
	(kill-ring-save (region-beginning) (region-end))
	(exchange-point-and-mark)
	(deactivate-mark))))

  (defun init/goto-line (&optional line)
    (interactive "p")
    (if line
	(goto-line line)
      (beginning-of-buffer)))

  ;; ------------------------------------------------------------

  (defun init/ryo-modal-setup ()
    (ryo-modal-keys
     ("z" ryo-modal-mode)

     (","
      (("h" help-command :name "Help")
       ("t"
	(("s" split-window-vertically)
	 ("v" split-window-horizontally)
	 ("q" delete-window)
	 ("d" delete-other-windows)
	 ("o" previous-window-any-frame)
	 ("t" next-window-any-frame)
	 ("m" windmove-left)
	 ("i" windmove-right)
	 ("n" windmove-down)
	 ("e" windmove-up)
	 ("M" shrink-window-horizontally)
	 ("I" enlarge-window-horizontally)
	 ("N" shrink-window)
	 ("E" enlarge-window)))))

     (":" execute-extended-command)

     ("." ryo-modal-repeat)

     ("m" backward-char)
     ("i" forward-char)
     ("n" next-line)
     ("e" previous-line)

     ("M" init/select-line-before-point)
     ("I" init/select-line-after-point)
     ("N" init/select-to-next-line)
     ("E" init/select-to-previous-line)

     ("w" forward-word)
     ("b" backward-word)

     ("W" init/select-to-forward-word)
     ("B" init/select-to-backward-word)

     ("s" forward-sexp)
     ("r" backward-sexp)

     ("S" init/select-to-forward-sexp)
     ("R" init/select-to-backward-sexp)

     ("g"
      (("m" back-to-indentation)
       ("M" move-beginning-of-line)
       ("i" move-end-of-line)
       ("n" forward-paragraph)
       ("e" backward-paragraph)

       ("w" forward-sentence)
       ("b" backward-sentence)

       ("g" init/goto-line)
       ("G" end-of-buffer)))

     ("f" isearch-forward)
     ("M-f" isearch-backward)

     ("v" set-mark-command)
     (";" init/deactivate-mark)
     ("M-;" exchange-point-and-mark)
     ("x" init/select-lines)

     ("y" init/kill-ring-save-selection)
     ("d" init/kill-selection)
     ("p" yank)
     ("u" undo)

     ("a" forward-char :exit t)
     ("A" move-end-of-line :exit t)
     ("Z" back-to-indentation :exit t)
     ("o" init/open-lines-below :exit t)
     ("O" init/open-lines-above :exit t)
     ("c" init/kill-selection :exit t))

    (define-key ryo-modal-mode-map (kbd ", x") ctl-x-map)

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

(use-package color-theme-sanityinc-tomorrow
  :config (load-theme 'sanityinc-tomorrow-night))

;; ------------------------------------------------------------

(use-package which-key
  :config (which-key-mode))

;; ------------------------------------------------------------

(use-package kotlin-mode)
