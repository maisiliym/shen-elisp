;; -*- lexical-binding: t -*-

;; [[file:shen-elisp.org::*Collecting%20KLambda%20files][Collecting\ KLambda\ files:2]]
(require 'shen-primitives)
(setq *klambda-directory-name* "KLambda")
(setq *klambda-directory* (file-name-as-directory (concat (file-name-directory load-file-name) *klambda-directory-name*)))
(setq *klambda-files*
      (mapcar (lambda (klFile) (concat *klambda-directory* klFile))
              '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl"
                "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl"
                "macros.kl" "declarations.kl" "types.kl" "t-star.kl")))
;; Collecting\ KLambda\ files:2 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:1]]
(defun get-klambda-sexp-strings (klambda-file)
  (with-temp-buffer
    (insert-file-contents klambda-file)
    (with-syntax-table (make-syntax-table lisp-mode-syntax-table)
      (modify-syntax-entry 59 "_") ;; semi-colon
      (modify-syntax-entry ?, "_")
      (modify-syntax-entry ?# "_")
      (modify-syntax-entry ?' "_")
      (modify-syntax-entry ?` "_")
      (let* ((klambda-code (buffer-string))
             (current-sexp-end (scan-lists 0 1 0))
             (groups nil))
        (progn
          (while current-sexp-end
            (let ((current-sexp-start (scan-lists current-sexp-end -1 0)))
              (progn
                (setq groups (nconc groups (list (buffer-substring current-sexp-start current-sexp-end))))
                (setq current-sexp-end (scan-lists current-sexp-end 1 0)))))
          groups)))))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:1 ends here

;; [[file:shen-elisp.org::*Modifying%20The%20Elisp%20Reader%20For%20KLambda][Modifying\ The\ Elisp\ Reader\ For\ KLambda:2]]
(defun remove-reserved-elisp-characters (klambda-sexp)
  (let ((InString nil)
        (illegal-characters
         (mapcar
          (lambda (char->spelling) (nth 0 char->spelling))
          shen/*illegal-character->spelling*))
        (res)
        (curr klambda-sexp))
    (cl-flet ((append-and-advance
               (&optional X)
               (progn
                 (if X (setq res (concat res X))
                   (setq res (concat res (substring curr 0 1))))
                 (setq curr (substring curr 1)))))
      (while (not (= 0 (length curr)))
        (cond
         ((char-equal (string-to-char curr) ?\")
          (if InString
              (progn
                (setq InString nil)
                (append-and-advance))
            (progn
              (setq InString 't)
              (append-and-advance))))
         ((memq (string-to-char curr) illegal-characters)
          (if InString
              (append-and-advance)
            (append-and-advance
             (car (assoc-default
                   (string-to-char curr)
                   shen/*illegal-character->spelling*)))))
         (t (append-and-advance))))
      res)))
;; Modifying\ The\ Elisp\ Reader\ For\ KLambda:2 ends here

;; [[file:shen-elisp.org::*Iterating%20over%20KLambda%20Files][Iterating\ over\ KLambda\ Files:1]]
(setq *temp-shen-buffer* (find-file-noselect "/tmp/shen.el"))
(setq *temp-shen-autoloads* (find-file-noselect "/tmp/shen-autoloads.el"))
(defun eval-klambda-files (klambda-files)
  (with-current-buffer *temp-shen-buffer*
    (progn
      (erase-buffer)
      (insert (format "%s\n" ";; -*- lexical-binding: t -*- "))
      (insert (format "%s\n" "(require 'shen-primitives)"))
      (goto-char (point-max))
      (dolist (klambda-file klambda-files nil)
        (eval-klambda-file klambda-file))
      (goto-char (point-max))
      (insert (format "%s\n" "(provide 'shen-shen)"))
      (save-buffer))))
(defun eval-klambda-file (klambda-file)
  (dolist (klambda-sexp-string (get-klambda-sexp-strings klambda-file) nil)
    (eval-klambda-sexp-string klambda-sexp-string)))
(defun eval-klambda-sexp-string (klambda-sexp-string)
  (let* ((ast (read (remove-reserved-elisp-characters klambda-sexp-string))))
    (shen/kl-to-buffer ast *temp-shen-buffer*)))
;; Iterating\ over\ KLambda\ Files:1 ends here

;; [[file:shen-elisp.org::*The%20Runner][The\ Runner:1]]
(defun load-klambda () (eval-klambda-files *klambda-files*))
(defun load-only ()
  (progn
    (load "/home/deech/Lisp/shen-elisp/primitives.el")
    (load "/home/deech/Lisp/shen-elisp/install.el")))
(defun runner ()
  (progn
    (setq max-lisp-eval-depth 60000)
    (setq max-specpdl-size 13000)
    (byte-compile-file "/home/deech/Lisp/shen-elisp/primitives.el")
    (load "/home/deech/Lisp/shen-elisp/primitives.elc")
    (byte-compile-file "/home/deech/Lisp/shen-elisp/install.el")
    (load "/home/deech/Lisp/shen-elisp/install.elc")
    (eval-klambda-files *klambda-files*)
    (byte-compile-file "/tmp/shen.el")
    (load "/tmp/shen.elc")
    (byte-compile-file "/home/deech/Lisp/shen-elisp/overlays.el")
    (load "/home/deech/Lisp/shen-elisp/overlays.elc")
    (byte-compile-file "/home/deech/Lisp/shen-elisp/repl.el")
    (load "/home/deech/Lisp/shen-elisp/repl.elc")
    (shen/shen.shen)))
;; The\ Runner:1 ends here
