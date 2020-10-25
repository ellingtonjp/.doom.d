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
(map! :leader "-" #'dired-jump-other-window)

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
  (setq org-modules '(org-drill)
        org-log-done 'time
        ;; org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "INPROGRESS(i)" "|" "DONE(d)" "CANCELED(c)"))
        ;; org-todo-keyword-faces
        ;; '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
        ;;   ("WAIT" :foreground "#9f7efe" :weight normal :underline t)
        ;;   ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
        ;;   ("DONE" :foreground "#50a14f" :weight normal :underline t)
        ;;   ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
  ;; )
        org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
)

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(map! :after evil-org
      :map evil-org-mode-map
      :n "_" #'org-previous-visible-heading
      :n "+" #'org-next-visible-heading)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(map! :leader "a" #'evil-avy-goto-char-timer)


(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚫" "⚫" "⚫" "⚫")))
  ;; (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(map! :desc "Create sparse tree for tags" :ne "SPC r t" #'org-tags-sparse-tree )

(use-package emojify
  :init
  (setq emojify-display-style "unicode")
  (setq emojify-emoji-set "twemoji-v2"))

(map! :desc "Insert emoji" :ne "SPC e" #'emojify-insert-emoji )

(add-hook 'dired-mode-hook 'org-download-enable)

(setq doom-theme 'doom-solarized-light)

(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))

(map! :desc "Autocorrect" :ne "SPC z" #'flyspell-auto-correct-word)

(map! :i "C-i" #'flyspell-auto-correct-word)
