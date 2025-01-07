;;; gptel-rewrite-master.el -*- lexical-binding: t; -*-

(require 'gptel-rewrite)

(defun gptel-rewrite-fix-grammar-errors ()
  "Rewrite the selected region or buffer to fix grammar errors"
  (interactive)
  (unless (use-region-p)
    (error "You should select a region to perform such action"))
  (let ((prompt "You are a researcher of computer science. Fix all grammar and spell mistakes in the paragraph. Do not explain your changes."))
    (gptel--suffix-rewrite prompt)))

(defun gptel-rewrite-improve-flow ()
  "Rewrite the selected region or buffer to fix flow problems"
  (interactive)
  (unless (use-region-p)
    (error "You should select a region to perform such action"))
  (let ((prompt "You are a researcher of computer science. Improve the flow of the paragraph of a PL(Programming Languages) paper and make it more readable and native. Do not explain your changes."))
    (gptel--suffix-rewrite prompt)))

(defun gptel-translate-to (lang)
  "Translate the selected region or buffer to the specified language.
LANG is the full name of the target language (e.g., `French')."
  (interactive
   (list (completing-read "Target language: " '("English" "French" "Spanish" "German" "Chinese" "Japanese" "Russian" "Italian" "Korean"))))
  (unless (use-region-p)
    (error "You should select a region to perform such action"))
  (let ((prompt (format "Translate the following text to %s. Do not explain your changes." lang)))
    (gptel--suffix-rewrite prompt)))

(transient-define-prefix gptel-translate-to-menu ()
  "Translate text to a specified language."
  [:description "Select target language:"
   ("e" "English" (lambda () (interactive) (gptel-translate-to "English")))
   ("f" "French" (lambda () (interactive) (gptel-translate-to "French")))
   ("s" "Spanish" (lambda () (interactive) (gptel-translate-to "Spanish")))
   ("g" "German" (lambda () (interactive) (gptel-translate-to "German")))
   ("c" "Chinese" (lambda () (interactive) (gptel-translate-to "Chinese")))
   ("j" "Japanese" (lambda () (interactive) (gptel-translate-to "Japanese")))
   ("r" "Russian" (lambda () (interactive) (gptel-translate-to "Russian")))
   ("i" "Italian" (lambda () (interactive) (gptel-translate-to "Italian")))
   ("k" "Korean" (lambda () (interactive) (gptel-translate-to "Korean")))])

(map! "M-p f" #'gptel-rewrite-improve-flow)
(map! "M-p g" #'gptel-rewrite-fix-grammar-errors)
(map! "M-p m" #'gptel-rewrite)
(map! "M-p i" #'gptel)
(map! "M-p t" #'gptel-translate-to-menu)
