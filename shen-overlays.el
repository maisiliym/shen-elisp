;; [[file:~/Lisp/shen-elisp/shen-elisp.org::*License][License:1]]
;; Copyright (c) 2015-2016 Aditya Siram. All Rights Reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause
;; License:1 ends here

;; [[file:~/Lisp/shen-elisp/shen-elisp.org::*Questions][Questions:1]]
(defun shen/y-or-n? (S)
  (progn
    (shen/shen.prhush (shen/shen.proc-nl S) (shen/stoutput))
    (let ((Input (format "%s" (read-from-minibuffer " (y/n) " ))))
      (cond
       ((string-equal Input "y") 'true)
       ((string-equal Input "n") 'false)
       (t (progn
            (shen/shen.prhush  "please answer y or n~%" (shen/stoutput))
            (shen/y-or-n? S)))))))

(defun shen/shen.pause-for-user nil
  (let ((Byte (read-from-minibuffer "")))
    (if (and (= 1 (length Byte)) (= (string-to-char Byte) ?^))
        (shen/simple-error "input aborted\n")
      (shen/nl 1))))
;; Questions:1 ends here

;; [[file:~/Lisp/shen-elisp/shen-elisp.org::*Changing%20Directories][Changing Directories:1]]
(defun shen/cd (Path)
  (if (shen/internal/shen->predicate (shen/= Path ""))
      (shen/set '*home-directory* "")
    (let ((PathString (concat Path "/")))
      (progn
        (setq default-directory PathString)
        (shen/set '*home-directory* PathString))
      PathString)))
;; Changing Directories:1 ends here

;; [[file:~/Lisp/shen-elisp/shen-elisp.org::*Provide%20it][Provide it:1]]
(provide 'shen-overlays)
;; Provide it:1 ends here
