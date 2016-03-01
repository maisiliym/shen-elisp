(setq *klambda-directory-name* "KLambda")
(setq *klambda-directory* (file-name-as-directory (concat (file-name-directory (buffer-file-name)) *klambda-directory-name*)))
(setq *klambda-files*
      (mapcar (lambda (klFile) (concat *klambda-directory* klFile))
              '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl"
                "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl"
                "macros.kl" "declarations.kl" "types.kl"
                "t-star.kl")))

(defun get-klambda-sexp-strings (klambda-file)
  (let* ((klambda-buffer (find-file-noselect klambda-file)))
    (with-current-buffer
        klambda-buffer
      (let* ((klambda-code (buffer-string))
             (current-sexp-end (scan-lists 0 1 0))
             (groups nil))
        (progn
          (while current-sexp-end
            (setq groups (nconc groups (list (buffer-substring (scan-lists current-sexp-end -1 0) current-sexp-end))))
            (setq current-sexp-end (scan-lists current-sexp-end 1 0)))
          (kill-buffer)
          groups)))))
(defun eval-klambda-sexp-string (klambda-sexp-string)
  (let ((ast (read klambda-sexp-string)))
    (shen/eval-kl ast)))
(defun eval-klambda-file (klambda-file)
  (dolist (klambda-sexp-string (get-klambda-sexp-strings klambda-file) nil)
    (eval-klambda-sexp-string klambda-sexp-string)))
(defun eval-klambda-files (klambda-files)
  (dolist (klambda-file klambda-files nil)
    (eval-klambda-file klambda-file)))
(defun load-klambda () (eval-klambda-files *klambda-files*))
