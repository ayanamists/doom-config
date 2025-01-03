;;; get-keys.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'bitwarden)

(cl-defstruct cache key value)

(defvar my-cache nil
  "A cache to store key-value pairs.")

(defun get-key-with-cache (f id)
  "Get the value for ID using function `f`, with caching."
  (let ((cached (cl-find id my-cache :key #'cache-key :test #'equal)))
    (if cached
        (cache-value cached)
      (let ((value (funcall f id)))  ; Use `funcall` to call the function `f`
        (push (make-cache :key id :value value) my-cache)
        value))))

(defun get-bitwarden-apikey/internal (id)
  (gethash "value"
           (aref (gethash "fields" (bitwarden-get-info-by-id id)) 0)))

(defun get-bitwarden-apikey (id)
  (get-key-with-cache #'get-bitwarden-apikey/internal id))
