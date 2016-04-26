;; [[file:shen-elisp.org::*Converting%20The%20Symbol%20Table][Converting\ The\ Symbol\ Table:1]]
(require 'shen-primitives)
(require 'shen-shen)
;; Converting\ The\ Symbol\ Table:1 ends here

;; [[file:shen-elisp.org::*Converting%20The%20Symbol%20Table][Converting\ The\ Symbol\ Table:2]]
(defun shen/migrate-symbol-table ()
  (let ((SymbolTable (shen/value 'shen.*symbol-table*)))
    (if (not (hash-table-p SymbolTable))
        (let ((NewTable (make-hash-table)))
          (dolist (Entry SymbolTable NewTable)
            (puthash (car Entry) (cdr Entry) NewTable))
          (shen/set 'shen.*symbol-table* NewTable))
      SymbolTable)))

(shen/migrate-symbol-table)
;; Converting\ The\ Symbol\ Table:2 ends here

;; [[file:shen-elisp.org::*Converting%20The%20Symbol%20Table][Converting\ The\ Symbol\ Table:3]]
(defun shen/shen.lookup-func
    (Name Table)
  (let ((Form (gethash Name Table)))
    (if (not Form)
        (shen/simple-error
         (shen/app Name " has no lambda expansion\n" 'shen.a))
      Form)))

(defun shen/shen.update-symbol-table
    (Name Arity)
  (let ((lambda-function
         (shen/eval-kl
          (shen/shen.lambda-form Name Arity))))
    (puthash Name lambda-function (shen/value 'shen.*symbol-table*))
    (shen/value 'shen.*symbol-table*)))
;; Converting\ The\ Symbol\ Table:3 ends here

;; [[file:shen-elisp.org::*Converting%20The%20Symbol%20Table][Converting\ The\ Symbol\ Table:4]]
(provide 'shen-post-process-klambda)
;; Converting\ The\ Symbol\ Table:4 ends here
