;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jonathan Ellington"
      user-mail-address "ellingtonjp@gmail.com")

;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-solarized-light)
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'light))

(setq doom-variable-pitch-font (font-spec :family "ETBembo" :height 240 :weight 'thin))

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
 :n ">"   #'find-file-other-window

 (:prefix ("w")
  :nv "a" #'ace-window)

 (:prefix ("TAB")
  :nv "TAB" #'+workspace/other
  :n  ";"   #'+workspace/display))

(after! ispell
  ;; add an option for both checking both English and Spanish
  ;; if you want to enable it:
  ;;    M-x ispell-change-dictionary
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,es"))

(add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :config
  (setq projectile-track-known-projects-automatically nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :config
  (setq org-directory "/Users/jonathanellington/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-fontify-todo-headline t)
  (setq org-fontify-done-headline t)
  (setq org-superstar-headline-bullets-list '(?◈ ?◆ ?◇))
  (setq org-superstar-item-bullet-alist
        '((?- . ?⚬)
          (?+ . ?⚬)
          (?* . ?⚬))))

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook 'writeroom-mode)

(custom-set-faces!
 '(org-document-title :foreground "#333333" :height 1.5 :weight bold)
 '(org-level-1 :foreground "#333333" :height 1.2 :weight bold)
 '(org-level-2 :foreground "#333333" :height 1.1 :weight bold)
 '(org-level-3 :foreground "#333333" :weight bold)
 '(org-level-4 :foreground "#333333" :weight bold)
 '(org-level-5 :foreground "#333333" :weight bold)
 '(org-level-6 :foreground "#333333" :weight bold)
 '(org-list-dt :foreground "#333333")

 ;; Make TODO headings smaller. For some reason DONE headings are the right
 ;; size, so just fix the TODOs smaller.
 ;;
 ;; 0.89 is the magic number to make it match DONE
 ;;
 ;; '(org-done :inherit 'default :weight bold) [not needed for some reason]
 '(org-todo :height 0.88 :weight bold)
 '(org-checkbox :height 1.0 :weight bold)
 '(org-link :underline t :foreground "#333333")

 ;; set the font for text /after/ the TODO/DONE keyword
 ;; want it to match the normal text, not big headline text
 '(org-headline-done :inherit 'default)
 '(org-headline-todo :inherit 'default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; splits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-window-split-and-focus ()
  (interactive)
  (let ((curr-value evil-split-window-below))
    (setq evil-split-window-below t)
    (evil-window-split)
    (setq evil-split-window-below curr-value)))

(defun evil-window-vsplit-and-focus ()
  (interactive)
  (let ((curr-value evil-vsplit-window-right))
    (setq evil-vsplit-window-right t)
    (evil-window-vsplit)
    (setq evil-vsplit-window-right curr-value)))

(map!
 :leader
 (:prefix ("w")
 :n "S" #'evil-window-split-and-focus
 :n "V" #'evil-window-vsplit-and-focus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geiser and scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-geiser-same-window ()
  (interactive)
  (let ((curr-value geiser-repl-use-other-window))
    (setq geiser-repl-use-other-window nil)
    (run-geiser 'guile)
    (setq geiser-repl-use-other-window curr-value)))

(defun run-geiser-in-new-vsplit()
  (interactive)
  (let ((curr-window (selected-window)))
    (evil-window-vsplit-and-focus)
    (run-geiser-same-window)
    (select-window curr-window)))

(map! :mode 'geiser-repl-mode :desc "switch to geiser repl" :ne "gz" #'switch-to-geiser)

(setq geiser-active-implementations '(guile))

(map!
 :map geiser-mode-map
 :after geiser-mode
 :n "gz" #'switch-to-geiser              :desc "switch to geiser repl"
 :localleader
 :n "r"  #'run-geiser-same-window        :desc "run geiser in current window"
 :n "R"  #'run-geiser-in-new-vsplit      :desc "run geiser in new window"
 (:prefix ("e" . "eval")
  :n "d" #'geiser-eval-definition        :desc "eval definition"
  :n "D" #'geiser-eval-definition-and-go :desc "eval definition and go"
  :n "s" #'geiser-eval-last-sexp         :desc "eval last sexp"
  :n "b" #'geiser-eval-buffer            :desc "eval buffer"
  :n "B" #'geiser-eval-buffer-and-go     :desc "eval buffer and go"
  :v "r" #'geiser-eval-region            :desc "eval region"
  :v "R" #'geiser-eval-region-and-go     :desc "eval region and go"))

(set-lookup-handlers! 'scheme-mode
  :documentation '(geiser-doc-symbol-at-point :async t))




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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open info in a side window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-info-vsplit ()
  (interactive)
  (with-popup-rules! nil
    (evil-window-vsplit)
    (call-interactively #'info)))

;; this creates a window and opens info in it
;; has the unfortunate side-effect that pressing 'q' to close info leaves the
;; window
;;
;; figure out how to use ui/popup to open this to the side
(map!
 :leader
 (:prefix ("h")
  :n "<C-i>" #'open-info-vsplit))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add a directory as a playlist
;; M-x emms-add-directory-tree

(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-info-functions '(emms-info-tinytag))

(defun emms-seek-backward-30s ()
  (interactive)
  (emms-seek -30))

(defun emms-seek-forward-30s ()
  (interactive)
  (emms-seek -30))

(defun emms-play-spanish-playlist ()
  (interactive)
  (emms-play-playlist "/Users/jonathanellington/iCloud Drive/spanish/lt.playlist"))

(map!
 :leader
 (:prefix ("e" . "emms")
  :nv "s" #'emms-play-spanish-playlist
  :nv "p" #'emms-pause
  :nv "h" #'emms-seek-backward
  :nv "l" #'emms-seek-forward
  :nv "H" #'emms-seek-backward-30s
  :nv "L" #'emms-seek-forward-30s
  :nv "j" #'emms-next
  :nv "k" #'emms-previous
  :nv "y" #'emms-play-playlist
  :nv "g" #'emms-playlist-mode-go))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq avy-timeout-seconds 0.2)
(setq avy-all-windows t)

(defun avy-goto-word-timer (&optional arg)
  "Like avy-goto-char-timer, but limits candidates to those beginning on a word
boundary. The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-action-goto
     (avy-with avy-goto-char-timer
       (avy-process
        (avy--read-candidates
         (lambda (input)
           (format "\\b%s" input))))))))

(map!
 :leader
 (:prefix ("a" . "avy")
  :nv "c" #'avy-goto-char-timer
  :nv "w" #'avy-goto-word-timer
  :nv "l" #'avy-goto-line
  :nv "h" #'avy-org-goto-heading-timer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.djcbsoftware.nl/code/mu/mu4e/Longer-configuration.html

;; after installing gccemacs, mu4e doesn't work by configuring
;; in init.el. Instead I added it to package.el. This line is necessary
;; for some reason.
(add-to-list 'load-path "/usr/local/Cellar/mu/1.4.14/share/emacs/site-lisp/mu/mu4e")

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(after! mu4e
  (setq mu4e-sent-folder   "/sent"
        mu4e-drafts-folder "/drafts"
        mu4e-refile-folder "/all"
        mu4e-trash-folder  "/trash"

        ;; still can't get images to show
        mu4e-view-show-images t

        mu4e-update-interval 300
        mu4e-get-mail-command "mbsync -a")

  ;; I originally wanted to use this for displaying HTML emails, but
  ;; it requires xwidgets which is unavailable on my emacs build
  ;;
  ;; But, it makes it so when scrolling through emails, the window
  ;; doesn't autofocus on the email, which is nice.
  (mu4e-views-mu4e-use-view-msg-method "html"))


;; fixes replying to xwidget messages from header buffer
(defun mu4e-xwidgets-compose-hack-advice (orig-fn &rest args)
  (cl-letf (((symbol-function #'mu4e-get-view-buffer)
             (lambda () "*xwidget webkit:  *")))
    (apply orig-fn args)))
(advice-add #'mu4e-compose :around #'mu4e-xwidgets-compose-hack-advice)

(setq auth-sources
      '(macos-keychain-generic
        macos-keychain-internet
        "/Users/jonathanellington/.emacs.d/.local/etc/authinfo.gpg"
        "~/.authinfo.gpg"))

;; configuration for sending mail
(after! smtpmail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        mu4e-view-show-images t
        smtpmail-smtp-service 587))

;; (setq mu4e-html2text-command 'mu4e-shr2text)
;;;; enable inline images
;;(setq mu4e-view-show-images t)
;;
;;;; use imagemagick, if available
;;(when (fboundp 'imagemagick-register-types)
;;  (imagemagick-register-types))
