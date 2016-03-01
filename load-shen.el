(setq shen/*kl-files*
      (let ((kl-dir "KLambda")
            (fully-qualified-kl-files))
        (dolist (kl-file '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl"
                           "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl"
                           "macros.kl" "declarations.kl" "types.kl"
                           "t-star.kl")
                         (nreverse fully-qualified-kl-files))
          (push (concat (file-name-as-directory kl-dir) kl-file) fully-qualified-kl-files))))

(defun shen/toggle (pred) (if pred nil 1))

(defun shen/translate-unreadable-char (char)
  (cond ((char-equal ?: char) "shen_colon")
        ((char-equal ?;
                     char) "shen_semicolon")
        ((char-equal ?, char) "shen_comma")
        (t (format "%c" char))))

(defun shen/translate-kl-buffer (buffer)
  (with-current-buffer buffer
    (let ((between-quotes))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((current-character (char-after)))
          (if (char-equal (aref "\"" 0) current-character)
              (setq between-quotes (shen/toggle between-quotes)))
          (if (not between-quotes)
              (progn (delete-char 1)
                     (insert (shen/translate-unreadable-char current-character)))
            (forward-char 1))))
      (goto-char (point-min)))))

(if (not (file-exists-p "shen.elc"))
    (let (shen-el-buffer (find-file-noselect "shen.el"))
      (dolist (kl-file shen/*kl-files* nil)
        (let ((kl-buffer (find-file-noselect kl-file)))
          (shen/translate-kl-bufferkl-buffer)))))
