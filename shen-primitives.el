(require 'cl-macs)

(defconst shen/prefix "shen/")

(defsubst shen/symbol-p (X)
  (not (or (consp X) (bufferp X) (vectorp X) (numberp X) (stringp X))))

(defsubst shen/intern (String)
  (intern String))

(defsubst shen/symbol->string (X)
  (symbol-name X))

(defun shen/internal/prefix-symbol (X)
  (if (shen/symbol-p X)
      (intern (concat shen/prefix (symbol-name X)))
    X))

(defun shen/internal/symbol-prefixed-p (X)
  (and (shen/symbol-p X) (string-prefix-p shen/prefix (symbol-name X))))

(defun shen/internal/unprefix-symbol (X)
  (if (shen/internal/symbol-prefixed-p X)
      (intern (substring (symbol-name X) (length shen/prefix)))
    X))

(defun shen/set (X Y)
  (set (intern (concat shen/prefix (symbol-name X)))
       (let ((prefixed))
         (or  (and (symbolp Y)
                   (not (shen/internal/symbol-prefixed-p Y))
                   (progn
                     (setq prefixed (shen/internal/prefix-symbol Y))
                     (or (boundp prefixed)
                         (fboundp prefixed)))
                   prefixed)
              Y))))

(defun shen/value (X)
  (condition-case ex
      (symbol-value (intern (concat shen/prefix (symbol-name X))))
    ('error (error (format "%s has not been assigned" X)))))

(shen/set '*home-directory* "")
(shen/set '*stoutput* standard-output)
(shen/set '*stinput* [()])
(shen/set '*language* "Elisp")
(shen/set '*implementation* system-configuration)
(shen/set '*porters* "Aditya Siram")
(shen/set '*release* emacs-version)
(shen/set '*port* 1.7)
(shen/set '*os* (symbol-name system-type))

(defsubst shen/internal/shen->predicate (X)
  (eq X 'true))
(defsubst shen/internal/predicate->shen (X)
  (if X (quote true) (quote false)))

(defmacro shen/if (X Y Z)
  `(if (eq ,X 'true) ,Y ,Z))
(defmacro shen/and (X Y) `(shen/internal/predicate->shen (and (eq ,X 'true) (eq ,Y 'true))))
(defmacro shen/or (X Y) `(shen/internal/predicate->shen (or (eq ,X 'true) (eq ,Y 'true))))

(defmacro shen/cond (&rest CASES)
  (let* ((predicates-quoted-cases
          (mapcar (lambda (predicate-result-pair)
                    (list (if (shen/symbol-p (nth 0 predicate-result-pair))
                              (list 'quote (nth 0 predicate-result-pair))
                            (list 'shen/internal/shen->predicate (nth 0 predicate-result-pair)))
                          (nth 1 predicate-result-pair)))
                  CASES))
         (fallthrough-added (append predicates-quoted-cases (list '(t (error "One of the cond predicates must be true."))))))
    `(cond ,@fallthrough-added)))

(defmacro shen/lambda (X Y)
  (if (eq X nil)
      `(lambda () ,Y)
    `(lambda (,X) ,Y)))

(defmacro shen/let (X Y Z)
  `(let ((,X ,Y)) ,Z))

(defmacro shen/defun (F Args Body) `(defun ,F ,Args ,Body))

(defun shen/internal/= (X Y)
  (cond ((and (stringp X) (stringp Y)) (string-equal X Y))  ;;; (ref:strings-and-numbers)
        ((and (numberp X) (numberp Y)) (= X Y))
        ((and (symbolp X) (symbolp Y)) (eq X Y))
        (t
         (or (equal X Y) ;;; (ref:obvious-equality-test)
             (cond
              ((and (consp X) (consp Y))
               (let ((LengthX (safe-length X))
                     (LengthY (safe-length Y)))
                 (and
                  (= LengthX LengthY)
                  (let ((SoFar 't)
                        (InnerListsX (list X))
                        (InnerListsY (list Y))
                        (FirstTime 't)
                        (CurrentIndex 0))
                    (while (and SoFar InnerListsX InnerListsY)
                      (let* ((CurrentListX (pop InnerListsX))
                             (CurrentListY (pop InnerListsY))
                             (Iterate
                              (lambda ()
                                (let ((I 0))
                                  (while (and SoFar (< I LengthX))
                                    (let* ((CurrentX (nth I CurrentListX))
                                           (CurrentY (nth I CurrentListY)))
                                      (cond
                                       ((not (equal (type-of CurrentX) (type-of CurrentY))) ;;; (ref:elements are of the same type)
                                        (setq SoFar nil))
                                       ((and (consp CurrentX) (consp CurrentY)) ;;; (ref:store the inner list)
                                        (progn
                                          (push CurrentX InnerListsX)
                                          (push CurrentY InnerListsY)))
                                       (t (setq SoFar (shen/internal/= CurrentX CurrentY)))) ;;; (ref:compare the elements)
                                      (setq I (1+ I))))))))
                        (if (not FirstTime)
                            (progn
                              (setq FirstTime nil)
                              (setq LengthX (safe-length CurrentListX))
                              (setq LengthY (safe-length CurrentListY))
                              (setq SoFar (= LengthX LengthY))
                              (funcall Iterate))
                          (funcall Iterate))))
                    SoFar))))
              ((and (hash-table-p X) (hash-table-p Y)) ;;; (ref:compare hash tables)
               (and (= (hash-table-count X) (hash-table-count Y))
                    (string=  ;;; (ref:hash table comparison)
                     (prin1-to-string X)
                     (prin1-to-string Y))))
              (t nil))))))

(defsubst shen/= (X Y)
  (shen/internal/predicate->shen (shen/internal/= X Y)))

(defmacro shen/freeze (X)
  `(function (lambda nil ,X)))
(defsubst shen/type (X MyType) (declare (ignore MyType)) X)

(defsubst shen/cons (A Rest)
  (cons A Rest))

(defsubst shen/hd (List)    (car List))
(defsubst shen/tl (List)    (cdr List))
(defsubst shen/cons? (List) (shen/internal/predicate->shen (consp List)))

(defun shen/str (X)
  (cond ((null X) (error "null is not an atom in Shen; str cannot convert it to a string.~%"))
        ((or (symbolp X) (functionp X)) (symbol-name X))
        ((numberp X) (number-to-string X))
        ((stringp X) X)
        ((and (bufferp X) (buffer-file-name X)) (buffer-name X))
        ((eq X standard-input) "standard-input")
        ((eq X standard-output) "standard-output")
        (t
         (error (format "%s is not an atom, stream or closure; str cannot convert it to a string." X)))))

(defsubst shen/pos (S N) (string (aref S N)))

(defsubst shen/tlstr (X) (substring X 1))

(defsubst shen/string? (S) (shen/internal/predicate->shen (stringp S)))
(defsubst shen/cn (Str1 Str2) (concat Str1 Str2))
(defsubst shen/n->string (N) (string N))
(defsubst shen/string->n (S) (string-to-char S))

(define-error 'shen/error "Shen error" 'error)
(defsubst shen/simple-error (E)
  (signal 'shen/error
          (if (stringp E)
              (list E)
            E)))
(defmacro shen/trap-error (X F)
  `(condition-case ex ,X ('error (funcall ,F ex))))
(defsubst shen/error-to-string (E) (format "%s" E))

(defsubst shen/absvector (N) (make-hash-table :size N :rehash-size 3.0 :test 'shen/internal/hash-table-test))
(defsubst shen/address-> (Vector N Value) (progn (puthash N Value Vector) Vector))
(defsubst shen/<-address (Vector N) (gethash N Vector))
(defsubst shen/absvector? (X) (shen/internal/predicate->shen (hash-table-p X)))

(define-hash-table-test
  'shen/internal/hash-table-test
  (lambda (X Y)
    (shen/internal/= X Y))
  (lambda (X)
    (cond
     ((numberp X) X)
     ((consp X) (sxhash (prin1-to-string X)))
     ((hash-table-p X)
      (sxhash (prin1-to-string X)))
     (t (sxhash X)))))

(defconst shen/multiplication-limit (floor (sqrt most-positive-fixnum)))
(defconst shen/addition-limit (floor (/ most-positive-fixnum 2)))

(defun shen/number-op (X Y max op)
  (cond
   ((and (integerp X) (integerp Y))
    (if (and (< X max)
             (> X (- max))
             (< Y max)
             (> Y (- max)))
        (apply op (list X Y))
      (apply op (list (float X) (float Y)))))
   ((and (floatp X) (numberp Y)) (apply op (list X (float Y))))
   ((and (numberp X) (floatp Y)) (apply op (list (float X) Y)))
   (t (error (format "Trying to %s. Both %s and %s must be numbers" op X Y)))))

(defsubst shen/* (X Y) (shen/number-op X Y shen/multiplication-limit #'*))
(defsubst shen/+ (X Y) (shen/number-op X Y shen/addition-limit #'+))
(defsubst shen/- (X Y) (shen/number-op X Y shen/addition-limit #'-))

(defsubst shen// (X Y)
  (cond
   ((or (not (numberp X)) (not (numberp Y)))
    (error (format "Both %s and %s must be numbers." X Y)))
   ((and (integerp X) (integerp Y))
    (let* ((Div (/ (float X) (float Y)))
           (Truncated (floor Div)))
      (if (= Truncated Div)
          Truncated
        Div)))
   (t (/ (float X) (float Y)))))

(defsubst shen/> (X Y)     (shen/internal/predicate->shen (> X Y)))
(defsubst shen/< (X Y)     (shen/internal/predicate->shen (< X Y)))
(defsubst shen/>= (X Y)    (shen/internal/predicate->shen (>= X Y)))
(defsubst shen/<= (X Y)    (shen/internal/predicate->shen (<= X Y)))
(defsubst shen/number? (N) (shen/internal/predicate->shen (numberp N)))

(defconst shen/2^16 65536)
(defun shen/get-time (Time)
  (cl-flet
      ((timespec-to-number (spec)
                           (let* ((high (nth 0 spec))
                                  (low (nth 1 spec)))
                             (+ (* high shen/2^16) low))))
    (cond ((eq Time 'run) (timespec-to-number (get-internal-run-time)))
          ((eq Time 'real)(timespec-to-number (current-time)))
          ((eq Time 'unix)(timespec-to-number (current-time)))
          (t (error (format "get-time does not understand parameter %s." Time))))))

(defsubst shen/streamp (X) (and (bufferp X) (buffer-file-name X)))

(defun shen/open (Path Direction)
  (let* ((Path (concat (file-name-as-directory (shen/value '*home-directory*))
                       (file-relative-name Path)))
         (Buffer (find-buffer-visiting Path)))
    (if Buffer
        (progn
          (with-current-buffer Buffer
            (goto-char (point-min)))
          Buffer)
      (cond
       ((equal Direction 'in)
        (if (not (file-exists-p Path))
            (error (format "Path does not exist: %s" Path))
          (progn
            (setq Buffer (find-file-noselect Path))
            (with-current-buffer
                Buffer
              (progn
                (setq buffer-read-only 't)
                (setq-local shen/shen-buffer 't)
                (goto-char (point-min))))
            Buffer)))
       ((equal Direction 'out)
        (progn
          (setq Buffer (find-buffer-visiting Path))
          (if (bufferp Buffer)
              (if (and (buffer-local-value 'buffer-read-only Buffer) (buffer-local-value 'shen/shen-buffer Buffer))
                  (error (format  "A stream to %s already open read-only. Call (close \"%s\") followed by (open \"%s\" 'out). " Path Path Path))
                Buffer)
            (progn
              (setq Buffer (find-file-noselect Path))
              (with-current-buffer Buffer
                (progn
                  (goto-char (point-max))
                  (setq-local shen/shen-buffer 't)))))))))))

(defun shen/close (Stream)
  (if (not Stream)
      (error "Stream is nil.")
    (if (and (local-variable-p 'shen/shen-buffer Stream)
             (buffer-local-value 'shen/shen-buffer Stream))
        (cond ((buffer-local-value 'buffer-read-only Stream) (kill-buffer Stream))
              (t (with-current-buffer
                     Stream
                   (progn
                     (write-file (buffer-file-name Stream))
                     (kill-buffer Stream)
                     '())))))))

(defun shen/write-byte (Byte &optional S)
  (if S
      (cond
       ((bufferp S)
        (if (not (local-variable-p 'buffer-read-only S))
            (error (format "Buffer %s is read-only." S))
          (if (buffer-local-value 'shen/shen-buffer S)
              (write-char Byte S)
            (error (format "Buffer %s was not opened by Shen." S)))))
       ((functionp S) ;; (ref:write-byte-function)
        (funcall S Byte))
       (t (write-char (shen/stoutput) Byte)))
    (funcall (shen/stoutput) Byte)))

(defun shen/read-byte (&optional S)
  (cond
   ((and (bufferp S) (buffer-file-name S))
    (with-current-buffer S
      (let ((current-byte))
        (if (eq (point) (point-max))
            -1
          (progn
            (setq current-byte (get-byte))
            (forward-char)
            current-byte)))))
   ((vectorp S) (if (not (aref S 0))
                    -1
                  (pop (aref S 0))))
   (t (error (format "Unrecognized stream format %s" S)))))

(defun shen/internal/lookup-with-default (KEY ALIST DEFAULT)
  (car (or (assoc-default KEY ALIST) (list DEFAULT))))

(defun shen/internal/get-element-at (path ast)
  (let ((res ast))
    (dolist (current-index (reverse path) res)
      (if (listp current-index)
          (setq res (nthcdr (car current-index) res))
        (setq res (nth current-index res))))))

(defun shen/internal/nset-element-at (path ast new-element)
  (if (not path)
      (setf ast new-element)
    (let ((place-fn)
          (path (reverse path))
          (make-place-fn
           (lambda (path target)
             (if (listp path)
                 `(nthcdr ,path ,target)
                 `(nth ,path ,target)))))
      (progn
        (dotimes (current-index (length path) nil)
          (setq place-fn
                (funcall make-place-fn
                         (nth current-index path)
                         (if (= current-index 0)
                             'ast
                           place-fn))))
        (if (or (consp new-element) (shen/symbol-p new-element))
            (eval `(setf ,place-fn (quote ,new-element)) 't)
          (eval `(setf ,place-fn ,new-element)) 't)
        ast))))

(defun shen/internal/find-all (X ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast)))
          (found 'shen/not-found))
      (while lists-left-to-search
        (let* ((search-candidate (car lists-left-to-search))
               (search-candidate-path (nth 0 search-candidate))
               (current-list (nth 1 search-candidate)))
          (progn
            (setq lists-left-to-search (cdr lists-left-to-search))
            (dotimes (current-index (length current-list) nil)
              (let ((current-element (nth current-index current-list))
                    (current-path (cons current-index search-candidate-path)))
                (if (equal X current-element)
                    (if (consp found)
                        (push current-path found)
                      (setq found (list current-path)))
                  (if (consp current-element)
                      (push `(,current-path ,current-element)
                            lists-left-to-search))))))))
      found)))

(defun shen/internal/list-containing-first-occurrence-of (list-pred ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast)))
          (found 'shen/not-found))
      (progn
        (while (and lists-left-to-search (eq found 'shen/not-found))
          (let* ((search-candidate (car lists-left-to-search))
                 (search-candidate-path (nth 0 search-candidate))
                 (current-list (nth 1 search-candidate))
                 (current-list-length (length current-list)))
            (if (funcall list-pred current-list)
                (setq found search-candidate-path)
              (progn
                (setq lists-left-to-search
                      (append
                       (let ((reversed-lists-in-current-list))
                         (dotimes (current-index current-list-length (reverse reversed-lists-in-current-list))
                           (if (consp (nth current-index current-list))
                               (setq reversed-lists-in-current-list
                                     (cons (list (cons current-index search-candidate-path)
                                                 (nth current-index current-list))
                                           reversed-lists-in-current-list)))))
                       (cdr lists-left-to-search)))))))
        found))))

(defun shen/internal/get-path-relative-to (parent-path path)
  (and (shen/internal/starts-with-path parent-path path)
       (shen/internal/path-slice path 0 (- (length path) (length parent-path)))))

(defun shen/internal/starts-with-path (parent-path path)
  (and (<= (length parent-path) (length path))
       (equal parent-path
              (shen/internal/path-slice path
                                        (- (length path)
                                           (length parent-path))))))

(defun shen/internal/get-path-parent (path) (cdr path))

(defun shen/internal/path-slice (path start &optional end)
  (let ((start-to-end (nthcdr start path))
        (res))
    (if end
        (dotimes (i (- (if (< end (length path))
                           end
                         (length path))
                       start)
                    (nreverse res))
          (push (nth i start-to-end) res))
      start-to-end)))

(defun shen/internal/modify-ast (ast paths tx-fn)
  (let ((deepest-first (sort paths (lambda (A B) (> (length A) (length B)))))
        (current-ast ast))
    (dolist (path deepest-first current-ast)
      (setq current-ast
            (shen/internal/nset-element-at path ast (funcall tx-fn path ast))))))

(defun shen/internal/dotted-pair? (X)
  (and (consp X) (not (consp (cdr X)))))

(defun shen/internal/partition (pred Xs)
  (let ((a)
        (b))
    (dotimes (i (length Xs) (list a b))
      (push (nth i Xs)
            (if (funcall pred (nth i Xs)) a b)))))

(defun shen/internal/filter (pred Xs &optional include-index)
  (let ((accum))
    (dotimes (i (length Xs) accum)
      (if (funcall pred (nth i Xs))
          (push (if include-index
                    (list (nth i Xs) i)
                  (nth i Xs))
                accum)))))

(defun shen/internal/index-of (pred Xs)
  (let ((found)
        (index 0))
    (while (and (not found) (< index (length Xs)))
      (progn
        (if (funcall pred (nth index Xs))
            (setq found index))
        (setq index (+ index 1))))
    found))

(defun shen/internal/delete-first-eq (needle Xs)
  (let ((index (shen/internal/index-of (lambda (X) (eq X needle)) Xs)))
    (if index
        (let ((current-index 0)
              (copy))
          (while (< current-index (length Xs))
            (progn
              (if (not (= current-index index))
                  (push (nth current-index Xs) copy))
              (setq current-index (1+ current-index))))
          (nreverse copy))
      Xs)))

(defun shen/internal/get-function-symbol-and-funcall-paths (ast)
  (let ((namespace-only)        ;; (ref:namespace-only)
        (quote-only)            ;; (ref:quote-only)
        (possibly-apply-function)) ;; (ref:possibly-apply-function)
    (if (not (consp ast))
        (if (shen/symbol-p ast)
            (list nil '(nil) '(nil) nil nil)
          (list nil nil nil nil nil))
      (let ((current-path)                     ;; (ref:current-path)
            (current-list ast)                 ;; (ref:current-list)
            (current-list-length (length ast)) ;; (ref:current-list-length)
            (current-index 0)                  ;; (ref:current-index)
            (locally-scoped-symbols)           ;; (ref:locally-scoped-symbols)
            (inner-lists)                      ;; (ref:inner-lists)
            (cond-predicate-action-p)
            (inner-lists-in-cond-form))        ;; (ref:inner-lists-in-cond-form)
        (while (or (< current-index current-list-length) ;; (ref:continue iterating)
                   inner-lists)
          (cond
           ((and (= current-index current-list-length) inner-lists) ;; (ref:sublists left)
            (progn
              (setq locally-scoped-symbols (nth 0 (car inner-lists)))
              (setq current-path (nth 1 (car inner-lists)))
              (setq cond-predicate-action-p (nth 2 (car inner-lists)))
              (setq inner-lists-in-cond-form nil)
              (setq inner-lists (cdr inner-lists))
              (setq current-list (shen/internal/get-element-at current-path ast))
              (setq current-index 0)
              (setq current-list-length (length current-list))))
           ((and (< current-index current-list-length)              ;; (ref:not a list)
                 (not (consp (nth current-index current-list))))
            (let ((current-token (nth current-index current-list)))
              (if (= 0 current-index)
                  (if (and (not (eq current-token 'nil))
                           (shen/symbol-p current-token))
                      (progn
                        (if (and (not (memq current-token locally-scoped-symbols))
                                 (not (eq current-token 'defun)))
                            (push (cons 0 current-path)
                                  namespace-only))
                        (cond
                         ((or (eq current-token 'lambda)
                              (eq current-token 'shen/lambda)) ;; (ref:lambda form)
                          (progn
                            (push (nth 1 current-list) locally-scoped-symbols)
                            (setq current-index 2)))
                         ((eq current-token 'defun) ;; (ref:defun form)
                          (progn
                            (push (cons 1 current-path) namespace-only)
                            (setq locally-scoped-symbols
                                  (append (nth 2 current-list) locally-scoped-symbols))
                            (setq current-index 3)))
                         ((or (eq current-token 'let)
                              (eq current-token 'shen/let))  ;; (ref:let form)
                          (progn
                            (push (nth 1 current-list) locally-scoped-symbols)
                            (setq current-index 2)))
                         ((or (eq current-token 'cond)
                              (eq current-token 'shen/cond)) ;; (ref:cond form)
                          (progn
                            (setq inner-lists-in-cond-form 't)
                            (setq current-index 1)))
                         (t
                          (progn
                            (if (not cond-predicate-action-p)
                                (push (list (cons 0 current-path)
                                            (memq current-token locally-scoped-symbols))
                                      possibly-apply-function))
                            (setq current-index 1)))))
                    (setq current-index (1+ current-index)))
                (if (and (not (eq current-token 'nil))
                         (shen/symbol-p current-token))
                    (progn
                      (if (not (memq current-token locally-scoped-symbols))
                          (push (cons current-index current-path)
                                quote-only))
                      (setq current-index (1+ current-index)))
                  (setq current-index (1+ current-index))))))
           ((and (< current-index current-list-length)             ;; (ref:a sublist)
                 (consp (nth current-index current-list)))
            (progn
              (if (and (= 0 current-index) (not cond-predicate-action-p))
                  (push (list (cons current-index current-path)
                              nil)
                        possibly-apply-function))
              (push (list locally-scoped-symbols
                          (cons current-index current-path)
                          inner-lists-in-cond-form)
                    inner-lists)
              (setq current-index (+ current-index 1))))
           (t nil)))
        (list namespace-only quote-only possibly-apply-function))))) ;; (ref:returns)

(setq shen/*primitive-macros*
      '(shen/if
        shen/and
        shen/or
        shen/cond
        shen/lambda
        shen/let
        defun
        shen/freeze
        shen/trap-error))

(defun shen/internal/apply-function (f args locally-scoped)
  (cond
   (locally-scoped       ;;(ref:higher-order function)
    `(shen/internal/apply-higher-order-function ,f (list ,@args)))
   ((consp f)            ;;(ref:a list)
    `(shen/internal/apply-function-expression ,f (list ,@args)))
   (t
    (if (fboundp 'shen/arity)
        (let ((arity (shen/internal/check-partial-application f (length args)))) ;; (ref:known arity)
          (if (= arity -1)
              `(,f ,@args)
            `(shen/internal/apply-partially (function ,f) (list ,@args))))
      `(,f ,@args)))))

(defun shen/internal/apply-higher-order-function (f args)
  (condition-case apply-ex (apply f args)
    ('void-function
     (shen/internal/apply-higher-order-function (shen/internal/prefix-symbol f) args))
    ('wrong-number-of-arguments
     (condition-case ex
         (let ((arity (shen/internal/check-partial-application f (length args))))
           (if (= arity -1)
               (signal (car apply-ex) (cdr apply-ex))
             (apply (eval (shen/internal/make-lambda-expression f arity (length args)) 't) args)))
       ('wrong-number-of-arguments
        (shen/internal/apply-incrementally f args))))))

(defun shen/internal/apply-function-expression (exp args)
  (condition-case ex (apply exp args)
    ('wrong-number-of-arguments (shen/internal/apply-incrementally exp args))))

(defun shen/internal/apply-partially (f args)
  (let ((arity (shen/internal/check-partial-application f (length args))))
    (if (= arity -1)
        (apply f args)
      (apply (eval (shen/internal/make-lambda-expression f arity (length args)) 't) args))))

(defun shen/internal/make-lambda-expression (f arity num-args) ;; (ref:curried lambda)
  (let* ((all-args (let ((single-apply-args)
                         (blast-apply-args))
                     (dotimes (i arity (list (reverse blast-apply-args)
                                             (reverse single-apply-args)))
                       (push (intern (concat "A" (number-to-string i)))
                             (if (and num-args (< i num-args))
                                 blast-apply-args
                               single-apply-args)))))
         (blast-apply-args (nth 0 all-args))
         (single-apply-args (nth 1 all-args))
         (expression `(apply (function ,f) (list ,@(append blast-apply-args single-apply-args)))))
    (dolist (arg (reverse single-apply-args) expression)
      (setq expression `(shen/lambda ,arg ,expression)))
    (if blast-apply-args
        `(lambda ,blast-apply-args ,expression)
      expression)))

(defun shen/internal/apply-incrementally (f args) ;; (ref:incremental application)
  (let ((result f)
        (current-args args))
    (while current-args
      (setq result (funcall result (car current-args)))
      (setq current-args (cdr current-args)))
    result))

(defun shen/internal/check-partial-application (f num-args)
  (let ((arity (condition-case ex (shen/arity (shen/internal/unprefix-symbol f)) ('error -1))))
    (cond
     ((eq -1 arity) -1)
     ((= arity num-args) -1)
     ((> num-args arity) -1)
     (t arity))))

(defun shen/internal/find-recursive-call-paths (function-name args ast)
  (if (not (consp ast))
      'shen/not-found
    (let ((lists-left-to-search `((() ,ast))) ;; (ref:lists-left-to-search)
          (found 'shen/not-found))  ;; (ref:tail-calls-found)
      (while lists-left-to-search
        (let* ((search-candidate (car lists-left-to-search))
               (search-candidate-path (nth 0 search-candidate))
               (current-list (nth 1 search-candidate))
               (current-list-length (length current-list))
               (current-head (car current-list))
               (push-if-list     ;; (ref:push-if-list)
                (lambda (indexes)
                  (mapc
                   (lambda (index)
                     (if (consp (nth index current-list))
                         (setq lists-left-to-search
                               (append lists-left-to-search
                                       (list
                                        (list (cons index search-candidate-path)
                                              (nth index current-list)))))))
                   indexes))))
          (progn
            (setq lists-left-to-search (cdr lists-left-to-search))
            (cond ((and (eq current-head function-name)
                        (= (length (cdr current-list)) (length args)))
                   (if (not (consp found))
                       (setq found (list search-candidate-path))
                     (push search-candidate-path found)))
                  ((eq current-head 'shen/cond)
                   (progn
                     (mapc
                      (lambda (action-index-pair)
                        (setq lists-left-to-search
                              (let ((path-to-action
                                     (append (list 1 (1+ (nth 1 action-index-pair)))
                                             search-candidate-path)))
                                (append lists-left-to-search
                                        (list
                                         (list path-to-action
                                               (nth 0 action-index-pair)))))))
                      (mapcar
                       (lambda (predicate-action-index)
                         (list (nth 1 (nth 0 predicate-action-index))
                               (nth 1 predicate-action-index)))
                       (shen/internal/filter  ;; (ref:cond-filter)
                        (lambda (predicate-action-pair)
                          (consp (nth 1 predicate-action-pair)))
                        (cdr current-list)
                        't)))))
                  ((eq current-head 'shen/if)
                   (if (= 4 current-list-length)
                       (funcall push-if-list '(2 3))
                     (funcall push-if-list '(2))))
                  ((eq current-head 'shen/trap-error)
                   (funcall push-if-list '(1 2)))
                  ((or (eq current-head 'shen/let)
                       (eq current-head 'defun))
                   (funcall push-if-list '(3)))
                  ((eq current-head 'shen/lambda)
                   (funcall push-if-list '(2)))
                  (t (funcall push-if-list (list (- current-list-length 1))))))))
      found)))

(defun shen/start-of-function-chain (tail-call-path ast)
  (let* ((from-the-top (reverse tail-call-path))
         (current-from-top-path)
         (path-left-to-tail-call (reverse tail-call-path))
         (start tail-call-path) ;; (ref:start-accumulator)
         (locally-scoped))
    (cl-flet ((append-and-advance
               (X &optional reset-start)
               (progn
                 (setq start
                       (if reset-start ;; (ref:reset-start)
                           tail-call-path
                         current-from-top-path))
                 (setq current-from-top-path
                       (append (reverse (shen/internal/path-slice path-left-to-tail-call 0 X))
                               current-from-top-path)
                       path-left-to-tail-call (shen/internal/path-slice path-left-to-tail-call X))

                 )))
      (while (not (equal current-from-top-path tail-call-path))
        (let* ((current-list (shen/internal/get-element-at current-from-top-path ast))
               (current-head (car current-list)))
          (cond
           ((or (not (shen/symbol-p current-head))
                (eq 'shen/if current-head))  ;; (ref:if-stop-recording)
            (append-and-advance 1 't))
           ((eq 'defun current-head)    ;; (ref:defun-stop-recording)
            (progn
              (setq locally-scoped (append (nth 2 current-list) locally-scoped))
              (append-and-advance 1 't)))
           ((or
             (eq 'shen/let current-head)
             (eq 'shen/lambda current-head)) ;;; (ref:let-or-lambda-stop-recording)
            (progn
              (setq locally-scoped (append (list (nth 1 current-list)) locally-scoped))
              (append-and-advance 1 't)))
           ((eq 'shen/cond current-head)     ;;; (ref:cond-stop-recording)
            (append-and-advance 2 't))
           ((eq 'shen/do current-head)       ;;; (ref:do-stop-recording)
            (append-and-advance 1 't))
           (t (append-and-advance 1)))))
      start)))

(defun shen/internal/get-tail-call-paths (ast)
  (let* ((function-name (nth 1 ast))
         (args (nth 2 ast))
         (body (nth 3 ast))
         (recursive-call-paths (shen/internal/find-recursive-call-paths function-name args body)))
    (if (eq recursive-call-paths 'shen/not-found)
        'shen/not-found
      (let ((accum))
        (dolist (tail-call-path recursive-call-paths (if accum (reverse accum) 'shen/not-found))
          (let* ((context (shen/start-of-function-chain tail-call-path body)))
            (if (equal context tail-call-path)
                (push (append tail-call-path (list 3)) accum))))))))

(defun shen/trampoline-body (ast)
  (let* ((args (nth 2 ast))
         (body (nth 3 ast))
         (tail-trampoline (make-symbol "tail-trampoline")))
    `(cl-flet ((,tail-trampoline ,args ,body))
       (let ((result (funcall (function ,tail-trampoline) ,@args)))
         (while (vectorp result)
           (setq result (apply (function ,tail-trampoline) (aref result 0))))
         result))))

(defun shen/internal/parse-ast (ast)
  (if (not (consp ast))
      (if (shen/symbol-p ast) (list 'quote ast) ast)
    (let* ((function-and-symbol-paths (shen/internal/get-function-symbol-and-funcall-paths ast)) ;;; (ref:paths)
           (namespace-only (nth 0 function-and-symbol-paths))
           (quote-only (nth 1 function-and-symbol-paths))
           (possibly-apply-function (nth 2 function-and-symbol-paths))
           (current-ast ast))
      (progn
        (shen/internal/namespace-and-quote current-ast namespace-only quote-only) ;;; (ref:quote and namespace)
        (let ((apply-function (shen/internal/filter
                               (lambda (path-local)
                                 (let ((token (shen/internal/get-element-at (nth 0 path-local) ast)))
                                   (not (memq token shen/*primitive-macros*))))
                               possibly-apply-function)))
          (if (eq (car current-ast) 'defun) ;;; (ref:defun form)
              (let* ((tail-call-paths (shen/internal/get-tail-call-paths ast)))
                (if (not (eq tail-call-paths 'shen/not-found))
                    (let ((not-in-tail-call apply-function)
                          (in-tail-call))
                      (progn
                        (dolist (path tail-call-paths nil)
                          (let* ((tco-non-tco-pair ;;; (ref:inside the recursive call)
                                  (shen/internal/partition
                                   (lambda (apply-function-path-local)
                                     (shen/internal/starts-with-path path (nth 0 apply-function-path-local)))
                                   not-in-tail-call))
                                 (funcalled-tco
                                  (let* ((normalized-paths
                                          (shen/internal/filter
                                           (lambda (path-local) (not (equal (nth 0 path-local) '(0))))
                                           (mapcar
                                            (lambda (in-tco-path-local)
                                              (list
                                               (shen/internal/get-path-relative-to path (nth 0 in-tco-path-local))
                                               (nth 1 in-tco-path-local)))
                                            (nth 0 tco-non-tco-pair))))
                                         (tail-call (shen/internal/get-element-at path current-ast)))
                                    (list
                                     path
                                     `(vector (list ,@(cdr (shen/internal/add-funcalls tail-call normalized-paths)))))))) ;;; (ref:package up the arguments)
                            (progn
                              (setq not-in-tail-call (nth 1 tco-non-tco-pair))
                              (push funcalled-tco in-tail-call))))
                        (dolist (path-tail-call in-tail-call nil)  ;;; (ref:Sub in the recurs marker)
                          (shen/internal/modify-ast current-ast (list (nth 0 path-tail-call))
                                                    (lambda (path current-ast) (nth 1 path-tail-call))))
                        (setq current-ast (shen/internal/add-funcalls current-ast not-in-tail-call)) ;;; (ref:rest of the function applications)
                        (setq current-ast `(defun ,(nth 1 current-ast) ,(nth 2 current-ast) ,(shen/trampoline-body current-ast))))) ;;; (ref:write out the defun)
                  (setq current-ast (shen/internal/add-funcalls current-ast apply-function)))
                current-ast)
            (progn
              (setq current-ast (shen/internal/add-funcalls current-ast apply-function))
              current-ast)))))))

(defun shen/internal/namespace-and-quote (ast namespace-only-paths quote-only-paths)
  (progn
    (shen/internal/modify-ast ast namespace-only-paths
                     (lambda (path ast)
                       (let ((element (shen/internal/get-element-at path ast)))
                         (if (not (shen/internal/symbol-prefixed-p element))
                             (shen/internal/prefix-symbol (shen/internal/get-element-at path ast))
                           element))))
    (shen/internal/modify-ast ast quote-only-paths
                     (lambda (path ast)
                       (list 'quote (shen/internal/get-element-at path ast))))
    ast))

(defun shen/internal/add-funcalls (ast apply-function)
  (let ((paths-only (mapcar (lambda (path-local) (nth 0 path-local)) apply-function)))
    (shen/internal/modify-ast ast (mapcar #'shen/internal/get-path-parent paths-only)
                     (lambda (path ast)
                       (let* ((current-funcalled-list (shen/internal/get-element-at path ast))
                              (function-name (car current-funcalled-list))
                              (function-arguments (cdr current-funcalled-list)))
                         (shen/internal/apply-function
                          function-name
                          function-arguments
                          (shen/internal/lookup-with-default (cons 0 path) apply-function nil)))))))

(defun shen/make-holed-context (tail-call-path function-chain-path ast)
  (let* ((function-chain (shen/internal/get-element-at function-chain-path ast))
         (tail-call (shen/internal/get-element-at tail-call-path ast))
         (tail-call-relative-path
          (shen/internal/path-slice tail-call-path 0
                  (- (length tail-call-path)
                     (length function-chain-path)))))
    (shen/internal/nset-element-at tail-call-relative-path function-chain 'shen/__hole__)))

(defun shen/used-in-context (context locally-scoped)
  (mapcar (lambda (symbol-index-pair)
            (nth 1 symbol-index-pair))
          (shen/internal/filter
           (lambda (v)
             (not (eq 'shen/not-found (shen/internal/find-all v context))))
           locally-scoped
           't)))

(defun shen/substitute-in-context (context locally-scoped-alist)
  (let ((current-context context))
    (dolist (locally-scoped-pair locally-scoped-alist current-context)
      (let* ((name (nth 0 locally-scoped-pair))
             (value (nth 1 locally-scoped-pair))
             (all-matching-paths (shen/internal/find-all name current-context)))
        (if (not (eq all-matching-paths 'shen/not-found))
            (dolist (path all-matching-paths nil)
              (shen/internal/nset-element-at path current-context value)))))))

(defun shen/internal/consolidate (ast matcher-fn collector-fn tx-fn)
  (let* ((current-ast ast)
         (location-containing-chain
          (shen/internal/list-containing-first-occurrence-of matcher-fn ast)))
    (while (not (eq location-containing-chain 'shen/not-found))
      (let ((current-chain (shen/internal/get-element-at location-containing-chain current-ast))
            (accum))
        (progn
          (while (funcall matcher-fn current-chain)
            (let ((collected (funcall collector-fn accum current-chain)))
              (setq accum (nth 0 collected))
              (setq current-chain (nth 1 collected))))
          (setq current-ast
                (shen/internal/nset-element-at
                 location-containing-chain
                 current-ast
                 (funcall tx-fn accum current-chain)))
          (setq location-containing-chain
                (shen/internal/list-containing-first-occurrence-of matcher-fn current-ast)))))
    current-ast))

(defun shen/internal/consolidate-cons (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (eq (nth 0 current-list) 'shen/cons)))
   (lambda (accum current-chain)
     (list (cons (nth 1 current-chain) accum)
           (nth 2 current-chain)))
   (lambda (accum remaining-chain)
     (if (eq remaining-chain 'nil)
         `(list ,@(reverse accum))
       `(append (list ,@(reverse accum)) ,remaining-chain)))))

(defun shen/internal/consolidate-@s (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (eq (nth 0 current-list) 'shen/@s)))
   (lambda (accum current-chain)
     (list (cons (nth 1 current-chain) accum)
           (nth 2 current-chain)))
   (lambda (accum remaining-chain)
     (list 'concat (cons 'concat (reverse accum)) remaining-chain))))

(defun shen/internal/consolidate-tl (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 2 (length current-list))
          (eq (nth 0 current-list) 'shen/tl)))
   (lambda (accum current-chain)
     (list (if (not accum) 1 (+ accum 1))
           (nth 1 current-chain)))
   (lambda (accum remaining-chain)
     (list 'nthcdr accum remaining-chain))))

(defun shen/internal/add-1+ (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (and (eq (nth 0 current-list) 'shen/+)
               (or (eq (nth 1 current-list) 1)
                   (eq (nth 2 current-list) 1)))))
   (lambda (accum current-list)
     (if (eq (nth 1 current-list) 1)
         (list (nth 2 current-list) nil)
       (list (nth 1 current-list) nil)))
   (lambda (accum remaining-chain)
     (list '1+ accum))))

(defun shen/internal/nil-to-null (ast)
  (shen/internal/consolidate
   ast
   (lambda (current-list)
     (and current-list
          (consp current-list)
          (eq 3 (length current-list))
          (and (eq (nth 0 current-list) 'shen/=)
               (or (eq (nth 1 current-list) 'nil)
                   (eq (nth 2 current-list) 'nil)))))
   (lambda (accum current-list)
     (if (eq (nth 1 current-list) 'nil)
         (list (nth 2 current-list) nil)
       (list (nth 1 current-list) nil)))
   (lambda (accum remaining-chain)
     `(shen/internal/predicate->shen (null ,accum)))))

(setq shen/internal/*performance-overrides*
      '((map . (defun shen/map (F Xs)
                (mapcar (lambda (X)
                          (shen/internal/apply-higher-order-function F (list X)))
                        Xs)))
       (shen.lazyderef . (defun shen/shen\.lazyderef
                             (X ProcessN)
                           (let ((Current X)
                                 (KeepLooking t))
                             (while KeepLooking
                               (shen/if
                                (shen/shen.pvar? Current)
                                (shen/let Value (shen/shen.valvector Current ProcessN)
                                          (shen/if (shen/= Value 'shen.-null-)
                                                   (setq KeepLooking nil)
                                                   (setq Current Value)))
                                (setq KeepLooking nil)))
                             Current)))
       (append . (defun shen/append (Xs Ys) (append Xs Ys)))
       (shen.string->bytes . (defun shen/shen.string->bytes (S)
                               (string-to-list S)))
       (sum . (defun shen/sum (Xs) (apply #'+ Xs)))
       (hash . (defun shen/hash (N Div) (sxhash N)))
       (shen.mod . (defun shen/shen.mod (N Div) (mod N Div)))
       (integer? . (defun shen/integer? (N) (shen/internal/predicate->shen (integerp N))))
       (abs . (defun shen/shen.abs (N) (abs N)))
       (nth . (defun shen/nth (I Xs) (nth I Xs)))
       (element? . (defun shen/element? (Element Xs)
                     (let ((SearchList Xs)
                           (Found nil)
                           (Length (length Xs))
                           (Current 0))
                       (while (and (not Found) SearchList)
                         (setq Found (shen/internal/= Element (pop SearchList))))
                       (shen/internal/predicate->shen Found))))
       (shen.compose . (defun shen/shen.compose
                           (Fs X)
                         (let ((Result X))
                           (dolist (F Fs Result)
                             (setq Result (funcall F Result))))))))

(setq shen/internal/*dict-overrides*
      '((shen.dict . (defun shen/shen\.dict
                         (Size)
                       (let ((Dict (shen/absvector 4))
                             (Contents (shen/absvector Size)))
                         (progn
                           (shen/address-> Dict 0 'dictionary)
                           (shen/address-> Dict 1 Size)
                           (shen/address-> Dict 2 0)
                           (shen/address-> Dict 3 Contents)
                           Dict))))
        (shen.dict-> . (defun shen/shen\.dict->
                           (Dict Key Value)
                         (let* ((Count (shen/shen\.dict-count Dict))
                                (Contents (shen/<-address Dict 3))
                                (Exists (shen/<-address Contents Key)))
                           (progn
                             (if (not Exists)
                                 (shen/address-> Dict 2 (1+ Count)))
                             (shen/address-> Contents Key Value)))))
        (shen.<-dict . (defun shen/shen\.<-dict
                           (Dict Key)
                         (let* ((Contents (shen/<-address Dict 3))
                                (Existing (shen/<-address Contents Key)))
                           (if (not Existing)
                               (shen/freeze (shen/simple-error "value not found"))
                             Existing))))
        (shen.dict-rm . (defun shen/shen\.dict-rm
                            (Dict Key)
                          (let* ((Count (shen/shen\.dict-count Dict))
                                 (Contents (shen/<-address Dict 3))
                                 (Exists (shen/<-address Contents Key)))
                            (if (not Exists)
                                Key
                              (progn
                                (remhash Key Contents)
                                (shen/address-> Dict 2 (1- Count))
                                Key)))))
        (shen.dict-keys . (defun shen/shen\.dict-keys
                              (Dict)
                            (let* ((Contents (shen/<-address Dict 3)))
                              (hash-table-keys Contents))))
        (shen.dict-values . (defun shen/shen\.dict-values
                                (Dict)
                              (let* ((Contents (shen/<-address Dict 3)))
                                (hash-table-values Contents))))
        (shen.dict-fold . (defun shen/shen\.dict-fold
                              (F Dict Acc)
                            (let ((Contents (shen/<-address Dict 3)))
                              (progn
                                (setq NewAcc Acc)
                                (maphash
                                 (lambda (Key Value)
                                   (setq NewAcc (shen/internal/apply-higher-order-function F (list Key Value NewAcc))))
                                 Contents)
                                NewAcc))))
        (put . (defun shen/put
                   (X Pointer Y Dict)
                 (let* ((Contents (shen/<-address Dict 3))
                        (X-Contents (shen/<-address Contents X)))
                   (if X-Contents
                       (progn
                         (puthash Pointer Y X-Contents)
                         Y)
                     (progn
                       (setq X-Contents (shen/absvector 100))
                       (puthash X X-Contents Contents)
                       (puthash Pointer Y X-Contents)
                       Y)))))
        (unput . (defun shen/unput
                     (X Pointer Dict)
                   (let* ((Contents (shen/<-address Dict 3))
                          (X-Contents (shen/<-address Contents X)))
                     (progn
                       (if X-Contents
                           (remhash Pointer X-Contents))
                       X))))
        (get . (defun shen/get
                   (X Pointer Dict)
                 (let* ((Contents (shen/<-address Dict 3))
                        (X-Contents (shen/<-address Contents X))
                        (Pointer-Contents (if X-Contents (shen/<-address X-Contents Pointer))))
                   (if (not Pointer-Contents)
                       (shen/simple-error "value not found")
                     Pointer-Contents))))))

(setq shen/internal/*namespacing-overrides*
      '((function . (defun shen/function (S)
                      (shen/shen\.lookup-func
                       (shen/internal/unprefix-symbol S))))))

(setq shen/internal/*bugfix-overrides*
      '((untrack . (defun shen/untrack (F)
                     (progn
                       (shen/set shen.*tracking*
                                 (shen/internal/delete-first-eq
                                  F
                                  (shen/value shen.*tracking*)))
                       (shen/eval (shen/ps F)))))))

(defun shen/internal/kl-to-elisp (Kl)
  (shen/internal/nil-to-null
   (shen/internal/add-1+
    (shen/internal/consolidate-tl
     (shen/internal/consolidate-@s
      (shen/internal/consolidate-cons (shen/internal/parse-ast Kl)))))))

(defun shen/eval-kl (X)
  (if (and (consp X) (eq (car X) 'defun))
      (progn
        (byte-compile (eval (shen/internal/kl-to-elisp (copy-tree X)) 't))
        (nth 1 X))
    (eval (shen/internal/kl-to-elisp X) 't)))

(defun shen/internal/add-overrides (overrides table)
  (mapc
   (lambda (override)
     (puthash (car override)
              (cdr override)
              table))
   overrides))

(setq shen/*overrides*
      (let ((table (make-hash-table :test 'equal)))
        (shen/internal/add-overrides
         (append
          shen/internal/*performance-overrides*
          shen/internal/*dict-overrides*
          shen/internal/*namespacing-overrides*
          shen/internal/*bugfix-overrides*)
         table)
        table))

(defun shen/patch-klambda (ast)
 (if (eq (car ast) 'defun)
       (let ((override (gethash (nth 1 ast) shen/*overrides*)))
         (or override
             (shen/internal/parse-ast ast)))
     (let ((patched (gethash ast shen/*overrides* )))
       (or patched
           (shen/internal/parse-ast ast)))))

(defun shen/kl-to-buffer (X B)
  (with-current-buffer B
    (save-excursion
      (goto-char (point-max))
      (insert (pp-to-string
               (shen/internal/nil-to-null
                (shen/internal/add-1+
                 (shen/internal/consolidate-tl
                  (shen/internal/consolidate-@s
                   (shen/internal/consolidate-cons
                    (shen/patch-klambda X)))))))))))

(provide 'shen-primitives)
