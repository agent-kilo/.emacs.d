(setq make-backup-files nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   '(which-key ryo-modal use-package color-theme-sanityinc-tomorrow)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun init/deactivate-mark ()
  (interactive)
  (deactivate-mark))

(defun init/open-lines-below (count)
  "Open COUNT lines below."
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)))

(defun init/open-lines-above (count)
  "Open COUNT lines above."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun init/select-lines (count)
  "Select COUNT lines from the current line."
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

(defun init/select-to-next-line ()
  (interactive)
  (unless (use-region-p) (set-mark (point)))
  (next-line))

(defun init/select-to-prev-line ()
  (interactive)
  (unless (use-region-p) (set-mark (point)))
  (previous-line))

(defun init/select-to-next-word ()
  (interactive)
  (unless (use-region-p) (set-mark (point)))
  (forward-word))

(defun init/select-to-prev-word ()
  (interactive)
  (unless (use-region-p) (set-mark (point)))
  (backward-word))

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


(add-hook 'prog-mode-hook #'(lambda () (ryo-modal-mode 1)))

(global-set-key (kbd "C-z") 'ryo-modal-mode)

(ryo-modal-keys

 ("z" ryo-modal-mode)

 ("SPC"
  (("h" help-command :name "Help")))

 (":" execute-extended-command)

 ("." ryo-modal-repeat)

 ("m" backward-char)
 ("i" forward-char)
 ("n" next-line)
 ("e" previous-line)

 ("M" init/select-line-before-point)
 ("I" init/select-line-after-point)
 ("N" init/select-to-next-line)
 ("E" init/select-to-prev-line)

 ("w" forward-word)
 ("b" backward-word)

 ("W" init/select-to-next-word)
 ("B" init/select-to-prev-word)

 ("g"
  (("m" back-to-indentation)
   ("M" move-beginning-of-line)
   ("i" move-end-of-line)
   ("n" forward-paragraph)
   ("e" backward-paragraph)

   ("w" forward-sentence)
   ("b" backward-sentence)

   ("g" beginning-of-buffer)
   ("G" end-of-buffer)
   ("t" beginning-of-buffer)
   ("b" end-of-buffer)))

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

 ("C-m" windmove-left)
 ("C-n" windmove-down)
 ("C-e" windmove-up)
 ("C-i" windmove-right))

(define-key ryo-modal-mode-map (kbd "SPC x") ctl-x-map)


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
 ("9" "M-9"))


(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)
(which-key-mode)
