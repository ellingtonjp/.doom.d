;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jonathan Ellington"
      user-mail-address "ellingtonjp@gmail.com")

(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'light))
;; (setq doom-theme 'doom-solarized-light)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/iCloud Drive/Notes")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :n "M-n" #'evil-multiedit-match-and-next
 :n "M-p" #'evil-multiedit-match-and-prev

 :leader
 :n "z"   #'flyspell-auto-correct-word
 :n "-"   #'dired-jump
 :n "a"   #'evil-avy-goto-char-timer

 (:prefix ("w")
  :nv "a" #'ace-window))

(setq avy-timeout-seconds 0.2)
(setq avy-all-windows t)

(after! ispell
  ;; add an option for both checking both English and Spanish
  ;; if you want to enable it:
  ;;    M-x ispell-change-dictionary
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es"))

(add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geiser and scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-geiser-same-window ()
  (interactive)
  (let ((curr-value geiser-repl-use-other-window))
    (setq geiser-repl-use-other-window nil)
    (run-geiser 'guile)
    (setq geiser-repl-use-other-window curr-value)))

(map! :mode 'geiser-repl-mode :desc "switch to geiser repl" :ne "gz" #'switch-to-geiser)

(setq geiser-active-implementations '(guile))

(map!
 :map geiser-mode-map
 :after geiser-mode
 :n "gz" #'switch-to-geiser              :desc "switch to geiser repl"
 :localleader
 :n "r"  #'run-geiser-same-window        :desc "run geiser in current window"
 :n "R"  #'run-geiser                    :desc "run geiser in new window"
 (:prefix ("e" . "eval")
  :n "d" #'geiser-eval-definition        :desc "eval definition"
  :n "D" #'geiser-eval-definition-and-go :desc "eval definition and go"
  :n "s" #'geiser-eval-last-sexp         :desc "eval last sexp"
  :n "b" #'geiser-eval-buffer            :desc "eval buffer"
  :n "B" #'geiser-eval-buffer-and-go     :desc "eval buffer and go"
  :v "r" #'geiser-eval-region            :desc "eval region"
  :v "R" #'geiser-eval-region-and-go     :desc "eval region and go"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy + which-key posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

