;;; -*- lexical-binding: t -*-

(defun restore-latex-workspace ()
  "Find the last modified .tex file and place it in the left window
and find the main.pdf file and place it in the right window."
  (interactive)
  (let* ((tex-files (seq-filter (lambda (buf)
                                  (string-suffix-p ".tex" (buffer-file-name buf)))
                                (buffer-list)))
         (last-modified-tex (car tex-files))
         (main-pdf (seq-find (lambda (buf)
                               (string-suffix-p ".pdf" (buffer-file-name buf)))
                             (buffer-list))))
    (when last-modified-tex
      (delete-other-windows)
      (set-window-buffer (selected-window) last-modified-tex)
      (when main-pdf
        (split-window-right)
        (set-window-buffer (next-window) main-pdf)))))

(defun latex-indent-region (start end)
  "Run latexindent on the region from START to END
replacing the region with the output."
  (interactive "r")
  (let ((output-buffer (generate-new-buffer "*latexindent-output*")))
    (shell-command-on-region start end "latexindent" output-buffer)
    (delete-region start end)
    (insert-buffer-substring output-buffer)
    (kill-buffer output-buffer)))
