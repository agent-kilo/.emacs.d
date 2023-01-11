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

(add-hook 'prog-mode-hook #'(lambda () (ryo-modal-mode 1)))

(global-set-key (kbd "C-z") 'ryo-modal-mode)
(ryo-modal-keys
 ("z" ryo-modal-mode)
 ("SPC"
  (("h" help-command :name "Help")))
 (":" execute-extended-command)
 ("." ryo-modal-repeat)
 ("m" backward-char)
 ("n" next-line)
 ("e" previous-line)
 ("i" forward-char)
 ("w" forward-word)
 ("b" backward-word)
 ("g"
  (("m" back-to-indentation)
   ("M" move-beginning-of-line)
   ("i" move-end-of-line)
   ("e" beginning-of-buffer)
   ("n" end-of-buffer)
   ("w" forward-sentence)
   ("b" backward-sentence)))
 ("f" isearch-forward)
 ("M-f" isearch-backward)
 ("v" set-mark-command)
 (";" init/deactivate-mark)
 ("M-;" exchange-point-and-mark)
 ("y" kill-ring-save)
 ("d" kill-region)
 ("p" yank)
 ("u" undo)
 ("a" forward-char :exit t)
 ("A" move-end-of-line :exit t)
 ("Z" back-to-indentation :exit t)
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
