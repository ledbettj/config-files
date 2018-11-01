(defun env-path ()
  "returns a list containing all the directorys in the user's PATH environmental
variable."
  (split-string (getenv "PATH") ":"))

(defun path-env-string (paths)
  "join the paths in the provided list into a string of the form x:y:z"
  (mapconcat 'identity paths ":"))

(defun path-contains-p (path)
  "returns t if path is in the user's PATH."
  (if (member path (env-path)) t nil))

(defun path-prepend (path)
  "append the given path to the PATH variable unless it's already present."
  (unless (or (path-contains-p path) (not (file-exists-p path)))
    (setenv "PATH"
      (path-env-string
        (append (list path) (env-path)))))
  (add-to-list 'exec-path path))

(defun path-append (path)
  "prepend the given path to the PATH variable unless it's already present."
  (unless (or (path-contains-p path) (not (file-exists-p path)))
    (setenv "PATH"
      (path-env-string
        (append (env-path) (list path)))))
  (add-to-list 'exec-path path t))


(cl-loop for (position . path) in prefs/ensure-paths do
         (cond
          ((eq 'prepend position) (path-prepend path))
          ((eq 'append  position) (path-append path))))
