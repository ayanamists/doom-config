;;; latex-additional/replace-dollar.el -*- lexical-binding: t; -*-
;;; FIXME: THIS FILE CONTAINS SOME BUG, USE IT CAREFULLY

(defvar original-buffer-name nil
  "Name of the original buffer before preview.")

(defun preview-replace-dollar-delimiters ()
  "Preview replacement of $$ and $$$$ with \\(\\) and \\[\\] in a new buffer."
  (interactive)
  (let ((original-buffer (current-buffer))
        (preview-buffer (get-buffer-create "*Preview Dollar Replacements*"))
        (nosubstitutions '("tikzpicture" "verbatim" "nosubblock"))
        (nosubs nil))
    (setq original-buffer-name (buffer-name original-buffer))
    (with-current-buffer preview-buffer
      (erase-buffer)
      (insert-buffer-substring original-buffer)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (thing-at-point 'line t)))
          (if (string-match "^\\s-*\\\\begin{\\(.*?\\)}" line)
              (setq nosubs (member (match-string 1 line) nosubstitutions)))
          (if (string-match "^\\s-*%\\s-*\\\\begin{\\(.*?\\)}" line)
              (setq nosubs (member (match-string 1 line) nosubstitutions)))
          (if (string-match "^\\s-*\\\\end{\\(.*?\\)}" line)
              (setq nosubs nil))
          (if (string-match "^\\s-*%\\s-*\\\\end{\\(.*?\\)}" line)
              (setq nosubs nil))
          (unless nosubs
            ;; Replace $$...$$ with \[...\]
            (while (re-search-forward "\\$\\$\\(.*?\\)\\$\\$" (line-end-position) t)
              (replace-match "\\\\[\\1\\\\]"))
            ;; Replace $...$ with \(...\)
            (while (re-search-forward "\\$\\([^$]*?\\)\\$" (line-end-position) t)
              (replace-match "\\\\(\\1\\\\)")))
          (forward-line 1)))
      ;; Switch to the preview buffer
      (pop-to-buffer preview-buffer)
      (special-mode)
      (insert "\n\nPress 'C-c C-c' to apply the changes, or 'C-c C-x' to cancel.\n"))))


(defun apply-replace-dollar-delimiters ()
  "Apply the dollar delimiter replacements from
the preview buffer to the original buffer."
  (interactive)
  (let ((preview-buffer (current-buffer))
        (original-buffer (get-buffer original-buffer-name)))
    (when original-buffer
      (with-current-buffer original-buffer
        (erase-buffer)
        (insert-buffer-substring preview-buffer))
      (kill-buffer preview-buffer)
      (message "Replacements applied."))))

(defun cancel-replace-dollar-delimiters ()
  "Cancel the dollar delimiter replacements preview."
  (interactive)
  (let ((preview-buffer (current-buffer)))
    (kill-buffer preview-buffer)
    (message "Replacements canceled.")))

(defun setup-preview-keybindings ()
  "Set up keybindings for the preview buffer."
  (local-set-key (kbd "C-c C-c") 'apply-replace-dollar-delimiters)
  (local-set-key (kbd "C-c C-x") 'cancel-replace-dollar-delimiters))

(add-hook 'special-mode-hook 'setup-preview-keybindings)
