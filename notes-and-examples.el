;;; notes-and-examples.el -*- lexical-binding: t; -*-

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  ;; (setq org-fancy-priorities-list '("⚫" "⚫" "⚫" "⚫")))
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(setq deft-directory "~/org"
      deft-extensions '("org" "text")
      deft-recursive t)

(map! :desc "Create sparse tree for tags" :ne "SPC r t" #'org-tags-sparse-tree )

; for drag and drop images into emacs?
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package! org-drill :after org)
(setq org-drill-add-random-noise-to-intervals-p t)

(map! :mode 'org-mode :v "s" #'scheme-send-region)


(evil-define-command evil-window-split-and-focus (&optional count file)
  ;; copy from evil/evil-commands.el and just removing the check on the
  ;; evil-split-window-below variable so focus is always changed, regardless
  ;; of value of variable
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count 'above)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(evil-define-command evil-window-vsplit-and-focus (&optional count file)
  ;; copy from evil/evil-commands.el and just removing the check on the
  ;; evil-split-window-right variable so focus is always changed, regardless
  ;; of value of variable
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count 'left)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(map! :desc "split window vertically, moving point" :ne "SPC w V" #'evil-window-vsplit-and-focus)
(map! :desc "split window horizontally, moving point" :ne "SPC w S" #'evil-window-split-and-focus)




(defun which-key-max-dimensions-function-proportional ()
   (cons (frame-height) (/ (frame-width) 2)))
(setq which-key-custom-popup-max-dimensions-function 'which-key-max-dimensions-function-proportional)


(use-package ivy-posframe
  :ensure t
  :diminish ivy-posframe-mode
  :custom-face
  (ivy-posframe ((t (:background "#333244"))))
  (ivy-posframe-border ((t (:background "#abff00"))))
  (ivy-posframe-cursor ((t (:background "#00ff00"))))
  :hook
  (ivy-mode . ivy-posframe-mode)
  :config
  ;; custom define height of post frame per function
  (setq ivy-posframe-height-alist '((swiper . 15)
                                    (find-file . 20)
                                    (counsel-ag . 15)
                                    (counsel-projectile-ag . 30)
                                    (t      . 25)))

  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-window-center)
          (complete-symbol . ivy-posframe-display-at-point)
          ;;(counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (counsel-M-x     . ivy-posframe-display-at-frame-center)
          (t               . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))
