;;; openrouter-model-list/get-model-list.el -*- lexical-binding: t; -*-
;;; TODO: `load!' from doom will delay the exec of `openrouter-ensure-model-list',
;;; so we might to manual load-file or open emacs twice to make it work.
;;; Any better solutions?
;;; TODO: test if `openrouter-schedule-daily-update' can work

(defvar openrouter-model-list-file
  (expand-file-name "openrouter-model-list/openrouter-model-list.el" doom-user-dir)
  "File path to store OpenRouter model list in the current directory.")

(defun openrouter-fetch-model-list ()
  "Fetch the model list from OpenRouter API and update `openrouter-model-list.el`."
  (url-retrieve
   "https://openrouter.ai/api/v1/models"
   (lambda (status)
     (if (plist-get status :error)
         (message "[OpenRouter] Failed to fetch model list: %s" (plist-get status :error))
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (let* ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (data (json-read))
              (models (mapcar (lambda (model) (intern (alist-get 'id model)))
                              (alist-get 'data data)))
              (output (concat ";; OpenRouter model list (auto-updated)\n"
                              "(defvar openrouter-model-list\n  '(\n"
                              (mapconcat (lambda (model) (format "    %S" model)) models "\n")
                              "\n  ))\n")))
         ;; Write directly to the file
         (with-temp-file openrouter-model-list-file
           (insert output))
         (message "[OpenRouter] Model list updated successfully.")
         (load-file openrouter-model-list-file))))))

(defun openrouter-ensure-model-list ()
  "Ensure that `openrouter-model-list.el` exists in the current directory;
create it if necessary."
  (unless (file-exists-p openrouter-model-list-file)
    (message "[OpenRouter] Model list file not found. Creating...")
    (openrouter-fetch-model-list)))

(defun openrouter-schedule-daily-update ()
  "Schedule a daily update of OpenRouter model list at 12:00 PM."
  (unless (get 'openrouter-schedule-daily-update 'scheduled)
    (let* ((now (current-time))
           (current-hour (string-to-number (format-time-string "%H" now)))
           (current-minute (string-to-number (format-time-string "%M" now)))
           (seconds-until-noon
            (if (or (> current-hour 12)
                    (and (= current-hour 12) (> current-minute 0)))
                (+ (* (- 24 (- current-hour 12)) 3600)
                   (* (- 60 current-minute) 60))
              (+ (* (- 12 current-hour) 3600)
                 (* (- 0 current-minute) 60)))))
      (run-at-time (if (= seconds-until-noon 0) 1 seconds-until-noon) 86400 'openrouter-fetch-model-list)
      (put 'openrouter-schedule-daily-update 'scheduled t)
      (message "[OpenRouter] Scheduled daily model list update at 12:00 PM."))))

;; Ensure model list file exists
(openrouter-ensure-model-list)

;; Schedule daily update at 12:00 PM
(openrouter-schedule-daily-update)
