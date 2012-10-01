;;; paths.el - make sure exec-path and $PATH contain the correct items.
;; John Ledbetter <john.ledbetter@gmail.com>

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
  (add-to-list 'exec-path path)
)

(defun path-append (path)
  "prepend the given path to the PATH variable unless it's already present."
  (unless (or (path-contains-p path) (not (file-exists-p path)))
    (setenv "PATH"
      (path-env-string
        (append (env-path) (list path)))))
  (add-to-list 'exec-path path t)
)

(path-prepend (expand-file-name "~/Local/Bin"))
(path-prepend (expand-file-name "~/Local/Scripts"))
(path-prepend (expand-file-name "~/.rbenv/shims"))
(path-prepend (expand-file-name "~/.rbenv/bin"))
(path-append  "/usr/local/bin")
