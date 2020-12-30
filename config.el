;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jonathan Ellington"
      user-mail-address "ellingtonjp@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; TODO: function for automatically calling doom sync and restarting emacs

(setq projectile-project-search-path '("~/tech/"))

(map! :leader "w a" #'ace-window)
(map! :leader "-" #'dired-jump)

(setq avy-timeout-seconds 0.2)
(setq avy-all-windows t)

;; EXAMPLES
;;
;; Create org-mode link
;; (defun make-youtube-link (youtube_id)
;;   (browse-url (concat "https://youtube.com/embed" youtube_id)))

;; (after! org
;;        (org-add-link-type "yt" #'make-youtube-link)
;;        )

(use-package! org-drill
  :after org)

(after! org
  (set-company-backend! 'org-mode nil))
;;   (setq
        ;; org-modules '(org-drill)
        ;; org-log-done 'time
        ;; org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "CURR(c)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d)" "CNCL(k)")
        ;;                     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        ;; org-todo-keyword-faces
        ;;         '(("[-]" . +org-todo-active)
        ;;           ("NEXT" . +org-todo-active)
        ;;           ("SOMEDAY" . +org-todo-onhold)
        ;;           ("[?]" . +org-todo-onhold)
        ;;           ("WAIT" . +org-todo-onhold)
        ;;           ("HOLD" . +org-todo-onhold)
        ;;           ("PROJ" . +org-todo-project))
        ;; org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "INPROGRESS(i)" "|" "DONE(d)" "CANCELED(c)"))
        ;; org-todo-keyword-faces
        ;; '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
        ;;   ("WAIT" :foreground "#9f7efe" :weight normal :underline t)
        ;;   ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
        ;;   ("DONE" :foreground "#50a14f" :weight normal :underline t)
        ;;   ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
  ;; )
;; )

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(map! :leader "a" #'evil-avy-goto-char-timer)

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

(use-package emojify)
;;   :init
;;   (setq emojify-display-style "unicode")
;;   (setq emojify-emoji-set "twemoji-v2"))

(map! :desc "Insert emoji" :ne "SPC e" #'emojify-insert-emoji )

(add-hook 'dired-mode-hook 'org-download-enable)

(map! :desc "Autocorrect" :ne "SPC z" #'flyspell-auto-correct-word)

;; (setq doom-theme 'doom-solarized-light)
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'light))
;; (setq doom-font (font-spec :family "Monaco" :size 12 :weight 'light))

(defun run-geiser-same-window ()
  (interactive)
  (let ((curr-value geiser-repl-use-other-window))
    (setq geiser-repl-use-other-window nil)
    (run-geiser 'guile)
    (setq geiser-repl-use-other-window curr-value)))

(setq geiser-active-implementations '(guile))
(map! :map geiser-mode-map
      :after geiser-mode
      :ne "gz" #'switch-to-geiser              :desc "switch to geiser repl"
      :localleader
      :n "r"  #'run-geiser-same-window        :desc ""
      :n "R"  #'run-geiser                    :desc ""
      (:prefix ("e" . "eval")
       :n "d" #'geiser-eval-definition        :desc "eval definition"
       :n "D" #'geiser-eval-definition-and-go :desc "eval definition and go"
       :n "s" #'geiser-eval-last-sexp         :desc "eval last sexp"
       :n "b" #'geiser-eval-buffer            :desc "eval buffer"
       :n "B" #'geiser-eval-buffer-and-go     :desc "eval buffer and go"
       :v "r" #'geiser-eval-region            :desc "eval region"
       :v "R" #'geiser-eval-region-and-go     :desc "eval region and go"))
(map! :mode 'geiser-repl-mode :desc "switch to geiser repl" :ne "gz" #'switch-to-geiser)
(map! :mode 'org-mode :v "s" #'scheme-send-region)

(map! :map scheme-mode-map
      :after smartparens-mode
      :n ">" #'sp-forward-slurp-sexp          :desc "")

(after! ispell
  ;; add an option for both checking both English and Spanish
  ;; if you want to enable it:
  ;;    M-x ispell-change-dictionary
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es"))

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

(map! :desc "" :ne "M-n" #'evil-multiedit-match-and-next)
(map! :desc "" :ne "M-p" #'evil-multiedit-match-and-prev)

(setq org-drill-add-random-noise-to-intervals-p t)

;; (defun which-key-max-dimensions-function-proportional ()
;;    (cons (frame-height) (/ (frame-width) 2)))
;; (setq which-key-custom-popup-max-dimensions-function 'which-key-max-dimensions-function-proportional)

(use-package which-key-posframe
  :after which-key
  :config
  (setq which-key-posframe-border-width 10)
  (set-face-attribute 'which-key-posframe-border
                      nil
                      :background nil)
  (which-key-posframe-mode))

(use-package ivy
  :config
  (setq ivy-height 10))

(defun ivy-posframe-get-size-proportional ()
  (list :height ivy-height ; make it match the height set in ivy
        :width (round (* (frame-width) 0.7))
        :min-height ivy-height
        :min-width (round (* (frame-width) 0.7))))

(use-package ivy-posframe
  :after ivy
  :custom-face (ivy-posframe ((t (:background nil)))) ; I think this makes the background whatever the default is
  :config
  (setq
   ivy-posframe-size-function 'ivy-posframe-get-size-proportional
   ivy-posframe-border-width 10)
  (ivy-posframe-mode 1))

;(use-package ivy-posframe
;  :ensure t
;  :diminish ivy-posframe-mode
;  :custom-face
;  (ivy-posframe ((t (:background "#333244"))))
;  (ivy-posframe-border ((t (:background "#abff00"))))
;  (ivy-posframe-cursor ((t (:background "#00ff00"))))
;  :hook
;  (ivy-mode . ivy-posframe-mode)
;  :config
;  ;; custom define height of post frame per function
;  (setq ivy-posframe-height-alist '((swiper . 15)
;                                    (find-file . 20)
;                                    (counsel-ag . 15)
;                                    (counsel-projectile-ag . 30)
;                                    (t      . 25)))
;
;  ;; display at `ivy-posframe-style'
;  (setq ivy-posframe-display-functions-alist
;        '((swiper          . ivy-posframe-display-at-window-center)
;          (complete-symbol . ivy-posframe-display-at-point)
;          ;;(counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;          (counsel-M-x     . ivy-posframe-display-at-frame-center)
;          (t               . ivy-posframe-display-at-frame-center)))
;  (ivy-posframe-mode 1))

