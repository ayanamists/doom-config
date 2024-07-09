;;; ob-haskell-hook.el -*- lexical-binding: t; -*-

(defun org-babel-interpret-haskell (body params)
  (require 'inf-haskell)
  (add-hook 'inferior-haskell-hook
            (lambda ()
              (setq-local comint-prompt-regexp
                          (concat haskell-prompt-regexp "\\|^λ?> "))))
  (let* ((session (cdr (assq :session params)))
         (result-type (cdr (assq :result-type params)))
         (is-example (assq :example params))
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:haskell params)))
         (session (org-babel-haskell-initiate-session session params))
	 (comint-preoutput-filter-functions
	  (cons 'ansi-color-filter-apply comint-preoutput-filter-functions))
         (raw (org-babel-comint-with-output
		  (session org-babel-haskell-eoe nil full-body)
                (insert (if is-example
                            (org-trim full-body)
                            (concat ":{\n" (org-trim full-body) "\n:}" )))
                (comint-send-input nil t)
                (insert org-babel-haskell-eoe)
                (comint-send-input nil t)))
         (results (mapcar #'org-strip-quotes
			  (cdr (member org-babel-haskell-eoe
                                       (reverse (mapcar #'org-trim raw)))))))
    (org-babel-reassemble-table
     (let ((result
            (pcase result-type
              (`output (mapconcat #'identity (reverse results) "\n"))
              (`value (car results)))))
       (org-babel-result-cond (cdr (assq :result-params params))
	 result (when result (org-babel-script-escape result))))
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colname-names params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rowname-names params))))))
