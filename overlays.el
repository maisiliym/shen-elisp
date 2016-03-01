;; [[file:shen-elisp.org::*Looking%20Up%20Functions][Looking\ Up\ Functions:1]]
(require 'shen-primitives)
;;;###autoload
(defun shen/function (S)
  (shen/shen\.lookup-func
   (shen/unprefix-symbol S)
   (shen/value 'shen\.*symbol-table*)))

;;;###autoload
(defun shen/append (Xs Ys) (append Xs Ys))

;;;###autoload
(defun shen/map (F Xs)
  (mapcar F Xs))

;;;###autoload
(defun shen/shen.string->bytes (S)
  (string-to-list S))

;;;###autoload
(defun shen/shen.sum (Xs) (apply #'+ Xs))

;;;###autoload
(defun shen/shen.mod (N Div) (mod N Div))
;; Looking\ Up\ Functions:1 ends here

;; [[file:shen-elisp.org::*Providing%20The%20Overlays][Providing\ The\ Overlays:1]]
(provide 'shen-overlays)
;; Providing\ The\ Overlays:1 ends here
