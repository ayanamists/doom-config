;;; $DOOMDIR/config.el -*- leical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; NOTE: see https://github.com/doomemacs/doomemacs/issues/6131#issuecomment-1051576882
;; should use float for font size
(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 13.0))

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

(load! "latex-additional/windows.el")
(load! "latex-additional/replace-dollar.el")

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; add spacemacs-like 'SPC n' keys
(map! :leader
  "0" 'winum-select-window-0-or-10
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  )

;; add spacemacs-like 'SPC SPC' key to `execute-extended-command'
(map! :leader "SPC" 'execute-extended-command)


;; add prettify symbols for latex
;; FIXME: it's so slow, try to fix it
;; (add-hook 'TeX-mode-hook 'magic-latex-buffer)
;;
;; FIXME: I cannot disable `rainbow-delimiters-mode' in TeX mode,
;; what happend?
(add-hook 'TeX-mode-hook
          (lambda () (prettify-symbols-mode t)
            (auto-fill-mode t)))

(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))

;; (after! tex
;;   (add-hook 'TeX-update-style-hook (lambda () (rainbow-delimiters-mode -1))))
;;

(defun get-together-ai-key ()
  (getenv "TOGETHER_AI_KEY"))

(use-package! gptel
  :config
  (setq!
    gptel-max-tokens 4096
    gptel-backend (gptel-make-openai "TogetherAI"         ;Any name you want
    :host "api.together.xyz"
    :key #'get-together-ai-key
    :stream t
    :models '("meta-llama/Llama-3-70b-chat-hf"))
    gptel-model "meta-llama/Llama-3-70b-chat-hf"))

(use-package codeium
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

;; (add-hook 'TeX-mode-hook (lambda () (setq completion-at-point-functions '(codeium-completion-at-point))))

(use-package! eaf
  :load-path "~/.config/emacs/site-lisp/emacs-application-framework"
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ; (eaf-browser-continue-where-left-off t)
  ; (eaf-browser-enable-adblocker t)
  ; (browse-url-browser-function 'eaf-open-browser)
  ; :config
  ; (defalias 'browse-web #'eaf-open-browser)
  ; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-evil)
  ;; REF: https://github.com/manateelazycat/lazycat-emacs/blob/f5348757b3c8a145d583712840349b108ff344cd/site-lisp/config/init-eaf.el#L132
  (setq eaf-webengine-default-zoom 2)
  ;; IMPORTANT, or eaf-pdf-viewer will extremely slow in old pdfs
  (setq eaf-pdf-dark-mode nil)

  ;; FIXME: seems `spc spc' don't work. What happen?
  (define-key key-translation-map (kbd "SPC")
      (lambda (prompt)
        (if (derived-mode-p 'eaf-mode)
            (pcase eaf--buffer-app-name
              ("browser" (if  eaf-buffer-input-focus
                             (kbd "SPC")
                           (kbd eaf-evil-leader-key)))
              ("pdf-viewer" (kbd eaf-evil-leader-key))
              ("image-viewer" (kbd eaf-evil-leader-key))
              (_  (kbd "SPC")))
          (kbd "SPC")))))

(use-package! rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-share-data-dir "/usr/share/rime-data")
  ;; NOTE: this dir should exist, or `rime' will not properly working
  ;; 雾凇拼音: https://github.com/iDvel/rime-ice
  ;; also, this seems not recommanded by emacs-rime page
  (setq rime-user-data-dir "/home/ayanamists/plum/package/iDvel/ice"))

;; Seems good sometimes, but normally bad
;; (setq evil-want-minibuffer t)

(setq ebib-file-associations '())

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))


;; custmize startup
;; matou sakura, 間桐 桜
(defun my-weebery-is-always-greater ()
  (let* ((banner '("⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⠤⠴⠐⠒⠒⠒⠒⠶⠤⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ "
                   "⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⠴⠚⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣽⣦⣄⣀⠀⠀⠀⠀⠀⠀⠀ "
                   "⠀⠀⠀⠀⠀⠀⠀⣠⠞⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠛⢻⣦⡀⠀⠀⠀⠀ "
                   "⠀⠀⠀⠀⠀⢀⣼⠅⠀⠀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⡄⠀⠀⠀ "
                   "⠀⠀⠀⠀⠀⣾⡏⠀⢠⠞⠁⠀⠀⡀⠀⠀⠀⡄⢨⡄⠀⠀⢰⡄⠀⠀⠀⠀⠀⠀⣾⣿⣿⡄⠀⠀ "
                   "⠀⠀⠀⠀⣸⠉⠀⣰⠏⣠⠄⢀⣼⠁⠀⠀⣼⡇⢸⡇⠀⣇⠈⢷⡐⣆⠀⠀⠀⣠⣿⣿⣿⣷⠀⠀ "
                   "⠀⠀⠀⢠⡇⣰⢰⡏⣰⡟⠀⣾⡏⠀⠀⣼⢿⡇⣼⡇⠀⣿⠀⣾⣇⢹⣆⡀⠀⣽⣿⣿⣿⣿⡄⠀ "
                   "⠀⠀⠀⢸⢠⣿⣾⢡⣿⠃⢸⣿⠃⠀⢰⡇⢸⠇⣧⡇⢰⣿⠀⣽⣿⢸⣿⣿⣶⣾⣿⣿⣿⣿⡇⠀ "
                   "⠀⠀⠀⠟⠈⣿⣿⢸⣿⡇⢸⣿⡀⠀⣿⢀⣿⠘⣽⣇⣾⣿⡀⣿⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⢳⡄ "
                   "⠀⠀⠀⡇⢀⣿⣿⣿⣇⣻⣾⣿⣧⠀⣷⣾⣿⣿⢿⣿⡿⢾⣿⣿⣸⣿⢿⣿⣿⣿⣿⣿⣿⡿⣿⣷ "
                   "⠀⠀⠀⠇⢸⣿⣿⣿⡇⠀⢻⣿⠘⣖⣿⣿⣿⠏⢸⣟⣁⣸⣟⠀⠸⠋⠉⣿⣿⣿⣿⣿⣿⠇⣿⣯ "
                   "⠀⠀⠀⠀⣼⡇⣿⣷⠛⢿⣿⣿⠄⠘⢿⡀⠋⠀⠙⠉⢸⣿⣿⣿⡷⣦⠀⠸⣿⣿⣿⣿⡿⠀⣿⣿ "
                   "⠀⠀⠀⠀⣿⠇⣿⡏⠀⠘⣻⣿⠀⠀⠀⠀⠀⠀⠀⠀⢨⣽⠿⣿⡇⠁⠀⠸⠟⢛⠛⣿⣇⣰⣿⣿ "
                   "⠀⠀⠀⠀⣿⠀⣿⡇⠀⠈⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣁⠀⣿⣿⡿⢻⣿ "
                   "⢿⣷⠀⢠⡿⠀⣿⣧⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠞⠁⢠⢁⣿⣿⠇⢸⣿ "
                   "⠀⠁⢀⣸⡇⠀⣿⣿⣧⡀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡟⠀⣠⣶⣿⣿⣿⠀⢸⣿ "
                   "⠀⠀⠈⣿⡇⠀⣿⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡴⢿⣿⣿⣿⣿⣿⣿⣿⠀⠀⢻ "
                   "⠀⠀⢠⣿⠀⠀⣿⣿⡏⣿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀⣀⡤⠖⠉⠀⢸⣿⣿⣿⣿⣿⣿⡇⠀⠀⢸ "
                   "⠀⠀⢸⡏⠀⣴⣿⣿⠃⣽⣿⢻⣿⣿⣦⠤⠤⠔⠒⠉⠁⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⠃⠀⠀⢸ "
                   "⠀⠀⢸⠇⢰⣿⣿⡷⠀⣽⣿⣾⠛⠯⣿⣤⣤⠤⠤⠤⠤⠤⠤⠤⠐⠛⠀⢿⣿⣿⣿⣿⣄⣀⣄⠸ "
                   "⠀⠀⠸⠀⣼⣸⣿⣧⣤⡿⠟⠋⠀⢰⣿⠙⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣿⣿⣿⣿⣿⡿⠀ "
                   "⠀⠀⡀⢀⣟⣻⡟⣿⠂⠀⠀⠀⢀⣾⡏⠀⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢉⣙⣿⣿⡇⠀ "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'my-weebery-is-always-greater)
