;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; auto switch themes
(package! auto-dark)

(package! magic-latex-buffer
  :recipe (:host github :repo "zk-phi/magic-latex-buffer"))

;; FIXME: unable in my ubuntu machine, why?
(package! codeium
  :recipe (:host github :repo "Exafunction/codeium.el"))

(package! screenshot
  :recipe (:host github :repo "tecosaur/screenshot"))

(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars"))

(package! himalaya
  :recipe (:host github :repo "dantecatalfamo/himalaya-emacs"))

(package! gptel
  :recipe (:host github :repo "ayanamists/gptel"))

(package! bitwarden
  :recipe (:host github :repo "seanfarley/emacs-bitwarden"))

(package! olivetti)

(package! rime)

(package! ebib)
(package! biblio)
(package! blamer)
;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet))

(package! leetcode)

(package! boogie-friends)

;; (package! markdown-ts-mode
;;   :recipe (:host github :repo "LionyxML/markdown-ts-mode"))

;; (package! treesit-auto)

;; (package! auctex-latexmk
;;   :recipe (:host github :repo "tom-tan/auctex-latexmk"))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
