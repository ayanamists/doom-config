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
(setq doom-font (font-spec :family "Iosevka" :size 13.0))
(setq doom-symbol-font (font-spec :family "JuliaMono" :size 13.0))

(after! doom-ui
  ;; see https://github.com/LionyxML/auto-dark-emacs/issues/64
  ;; this problem seems quite complex
  (setq custom-safe-themes t) ;; this line is added to solve the problem
  (setq! auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
 (auto-dark-mode))

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
(load! "gptel-rewrite-master.el")
(load! "get-keys.el")
(load! "openrouter-model-list/get-model-list.el")
(load! openrouter-model-list-file)

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; add spacemacs-like 'SPC n' keys
;; (map! :leader
;;   "0" 'winum-select-window-0-or-10
;;   "1" 'winum-select-window-1
;;   "2" 'winum-select-window-2
;;   "3" 'winum-select-window-3
;;   "4" 'winum-select-window-4
;;   )

;; add spacemacs-like 'SPC SPC' key to `execute-extended-command'
(map! :leader "SPC" #'execute-extended-command)

;; use C-<spc> to for toggle-input-method
(map! "C-SPC" #'toggle-input-method)

;; add prettify symbols for latex
;; FIXME: it's so slow, try to fix it
;; (add-hook 'TeX-mode-hook 'magic-latex-buffer)
;;
;; FIXME: I cannot disable `rainbow-delimiters-mode' in TeX mode,
;; what happend?
(add-hook 'TeX-mode-hook
          (lambda ()
            (prettify-symbols-mode t)
            (setq prettify-symbols-unprettify-at-point t)
            (auto-fill-mode t)))

(add-hook 'org-mode-hook (lambda () (auto-fill-mode t)))

;; (after! tex
;;   (add-hook 'TeX-update-style-hook (lambda () (rainbow-delimiters-mode -1))))

(defun get-together-ai-key ()
  (getenv "TOGETHER_AI_KEY"))

(defun get-open-router-key ()
  (get-bitwarden-apikey "2b596056-1cbd-4421-8067-b28300727dce"))

(defun get-deepseek-key ()
  (get-bitwarden-apikey "df258464-6b65-4f8d-9a7a-b25a008504b1"))

(setq ai-openrouter
     (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key #'get-open-router-key
      :models openrouter-model-list))

(setq ai-deepseek
      (gptel-make-openai "DeepSeek"
       :host "api.deepseek.com"
       :endpoint "/chat/completions"
       :stream t
       :key #'get-deepseek-key
       :models '(deepseek-chat deepseek-coder)))

(use-package! gptel
  :config
  (setq!
    gptel-max-tokens 4000
    gptel-backend ai-openrouter
    gptel-model 'deepseek/deepseek-chat))

;; see https://github.com/karthink/gptel/issues/128
(defun my/gptel-write-buffer ()
  "Save buffer to disk when starting gptel"
  (unless (buffer-file-name (current-buffer))
    (let ((name (or (buffer-name (current-buffer)) "gptel"))
          (suffix (format-time-string "%Y%m%dT%H%M" (current-time)))
          (chat-dir "~/repo/chats")
          (ext (cond ((eq gptel-default-mode 'markdown-mode) ".md")
                     ((eq gptel-default-mode 'org-mode) ".org")
                     (t ".txt"))))
      (unless (file-directory-p chat-dir)
        (make-directory chat-dir :parents))
      (write-file (expand-file-name (concat name "-" suffix ext) chat-dir)))))

(add-hook 'gptel-mode-hook #'my/gptel-write-buffer)
(add-hook 'gptel-mode-hook
          (lambda () (evil-define-key 'normal
                       'gptel-mode (kbd "?") #'gptel-menu)))

;; accept completion from copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; (use-package codeium
;;   :init
;;   ;; use globally
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

;; FIXME Weird behavior on my Ubuntu 24.04, what happen?
;; (use-package! holo-layer
;;   :load-path "~/.config/emacs/site-lisp/holo-layer"
;;   :config
;;   (setq holo-layer-enable-indent-rainbow t)
;;   (holo-layer-enable)
;;   )

;; (use-package! eaf
;;   :load-path "~/.config/emacs/site-lisp/emacs-application-framework"
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   ; (eaf-browser-continue-where-left-off t)
;;   ; (eaf-browser-enable-adblocker t)
;;   ; (browse-url-browser-function 'eaf-open-browser)
;;   ; :config
;;   ; (defalias 'browse-web #'eaf-open-browser)
;;   ; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   ; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
;;   :config
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-evil)
;;   ;; REF: https://github.com/manateelazycat/lazycat-emacs/blob/f5348757b3c8a145d583712840349b108ff344cd/site-lisp/config/init-eaf.el#L132
;;   (setq eaf-webengine-default-zoom 2)
;;   ;; IMPORTANT, or eaf-pdf-viewer will extremely slow in old pdfs
;;   (setq eaf-pdf-dark-mode nil)

;;   ;; FIXME: seems `spc spc' don't work. What happen?
;;   (define-key key-translation-map (kbd "SPC")
;;       (lambda (prompt)
;;         (if (derived-mode-p 'eaf-mode)
;;             (pcase eaf--buffer-app-name
;;               ("browser" (if  eaf-buffer-input-focus
;;                              (kbd "SPC")
;;                            (kbd eaf-evil-leader-key)))
;;               ("pdf-viewer" (kbd eaf-evil-leader-key))
;;               ("image-viewer" (kbd eaf-evil-leader-key))
;;               (_  (kbd "SPC")))
;;           (kbd "SPC")))))

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

; Seems bad on two windows (left and right for example)
; (use-package blamer
;   :bind (("s-i" . blamer-show-commit-info))
;   :defer 20
;   :custom
;   (blamer-idle-time 0.3)
;   (blamer-min-offset 70)
;   :custom-face
;   (blamer-face ((t :foreground "#7a88cf"
;                     :background nil
;                     :height 140
;                     :italic t)))
;   :config
;   (global-blamer-mode 1))


(set-fontset-font t 'braille
   (font-spec :family "JuliaMono" :spacing 'M))

(defun read-lines-to-list (file-path)
  "Read the contents of FILE-PATH and return a list of lines."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun create-anime-banner ()
  (let* ((banner (read-lines-to-list "~/.config/doom/ascii-waifu/matou-sakura.txt"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'create-anime-banner)

;; see https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-867525845
;; problem with vterm and evil
(use-package vterm
  :config
  (advice-add #'vterm--redraw :after (lambda (&rest args) (evil-refresh-cursor evil-state)))
)

;; use lsp-brigde for sharp code completion
;; (use-package! lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log nil)
;;   (setq acm-enable-codeium t)
;;   (setq acm-enable-icon t)
;;   (setq acm-enable-preview t)
;;   (global-lsp-bridge-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (Haskell . t)))

;; (with-eval-after-load "ob-haskell"
;;   (load! "ob-haskell.el")
;;   (load! "ob-haskell-hook.el"))

;; On my iPad (with Termius), the modeline will cause display problem
;; And emacs cannot work properly unless switch off `doom-modeline-mode'
;; FIXME a temporial solution
(defun disable-modeline-when-terminal ()
  (when (not (display-graphic-p))
    (doom-modeline-mode -1)))
(add-hook 'window-setup-hook #'disable-modeline-when-terminal)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*leetcode" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(setq leetcode-prefer-language "scala")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/repo/jupyter_work/leetcode/solutions")

(add-hook 'scala-mode-hook
          (lambda () (flycheck-mode -1)))

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))

(use-package! indent-bars
  :hook (prog-mode . indent-bars-mode)) ; or whichever modes you prefer

;; It seems we should not switch to the native treesit.el
;; Because there're still many problems and the syntax highlight is not very good
;;
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (delete 'markdown treesit-auto-langs)
;;   (global-treesit-auto-mode))

;; (use-package markdown-ts-mode
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :defer 't
;;   :config
;;   (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
;;   (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;; EMAIL SETTINGS
(setq message-send-mail-function 'smtpmail-send-it)
(setq user-mail-address "ayanamists@gmail.com")
(setq user-full-name "Chenxi Li")

(setq smtpmail-smtp-user "ayanamists@gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(use-package! himalaya)

;; TS
(add-hook 'typescript-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)))
(add-hook 'typescript-tsx-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)))

;; Dafny
;; FIXME some error in dafny lsp
;; See https://github.com/boogie-org/boogie-friends/issues/42
(add-hook 'dafny-mode-hook #'lsp-deferred)

;; force AUCTeX to use pdftools
(after! tex
  (setq TeX-view-program-selection
        '((output-pdf "PDF Tools")
          (output-html "xdg-open")
          ((output-dvi has-no-display-manager) "dvi2tty")
          ((output-dvi style-pstricks) "dvips and gv")
          (output-dvi "xdvi"))))

;; modeline settings
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-file-name-style 'file-name)
(setq doom-modeline-hud t)
(setq doom-modeline-height 1)
(let ((font "JuliaMono"))
  (custom-set-faces
  '(mode-line ((t (:family font :height 0.93))))
  '(mode-line-active ((t (:family font :height 0.93)))) ; For 29+
  '(mode-line-inactive ((t (:family font :height 0.93))))))

(defun open-file-in-right-window ()
  "Open file in right window, creating it if necessary."
  (interactive)
  ;; Check if there's already a right window
  (let ((right-window (window-in-direction 'right)))
    (unless right-window
      ;; If no right window, split the current one vertically
      (split-window-right))
    ;; Switch to the right window and open file with 'find-file'
    (windmove-right)
    (call-interactively 'find-file)
    ;; Return focus to the left window
    (windmove-left)))


;; Doom requires each :lang module to set this variable,
;; but they may not always set it correctly.
(after! projectile
  (setq projectile-project-root-files
        '(".projectile"
          "rebar.config"
          "project.clj"
          "build.boot"
          "package.json"
          "deps.edn"
          "SConstruct"
          "default.nix"
          "flake.nix"
          "pom.xml"
          "build.sbt"
          "build.sc"
          "gradlew"
          "build.gradle"
          ".ensime"
          "Gemfile"
          "requirements.txt"
          "setup.py"
          "tox.ini"
          "composer.json"
          "Cargo.toml"
          "mix.exs"
          "stack.yaml"
          "dune-project"
          "info.rkt"
          "DESCRIPTION"
          "TAGS"
          "GTAGS"
          "configure.in"
          "configure.ac"
          "cscope.out"
          "Makefile"
          "CMakeLists.txt"
          "WORKSPACE"
          "debian/control")))

(use-package! bitwarden
  :config
  (setq! bitwarden-automatic-unlock
      (let* ((matches (auth-source-search :user "ayanamists@gmail.com"
                                          :host "bitwarden.com"
                                          :require '(:secret)
                                          :max 1))
             (entry (nth 0 matches)))
        (plist-get entry :secret))))
