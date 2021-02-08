#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: mbir.scm,v 1.6 2020-10-10 14:16:47-07 - - $
;;
;; Authors:
;;  Scott Melero   
;;  Korbie Sevilla 
;;
;; DESCRIPTION
;;  The file mentioned in argv[1] is read and assumed to be an mbir
;;  program, which is the executed. 

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))
(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))
(define *array-table*    (make-hash))
(define *label-table*    (make-hash))

;; set table entires
(define (set-function key value)
    (hash-set! *function-table* key value))

(define (set-label key value)
    (hash-set! *label-table* key value))

(define (set-var key value)
    (hash-set! *var-table* key value))

(define (set-array key value)
    (hash-set! *array-table* key value))

(for-each
    (lambda (symfun) (set-function (car symfun) (cadr symfun)))
    `(
        (+     ,+)
        (-     ,-)
        (*     ,*)
        (!=      ,(lambda (a b) (if (not(= a b)) #t #f)))
        (/     ,/)
        (<=    ,<=) 
        (>=    , >=) 
        (=     ,=) 
        (>     ,>) 
        (<     ,<)
        (^     ,expt)
        (sqrt  ,sqrt)
        (sqr   ,sqr)
        (nan   , (/ 0.0 0.0))
        (log   ,log) 
        (atan  ,atan)
        (asin  ,asin)
        (acos  ,acos)
        (sin   ,sin)
        (cos   ,cos) 
        (tan   ,tan)
        (ceil  ,ceiling) 
        (exp   ,exp) 
        (floor ,floor)
        (abs   ,abs )
        (round ,round)
    ))

(define *var-table*      (make-hash))
(for-each (lambda (var) (set-var (car var) (cadr var)))
   `(
        (e    ,(exp 1.0))
        (eof  0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    ))

; scan all of the lines in the program.
; For any line which has a label, add to label table
; along with the top-level node that points at the label.
(define (find-labels program)
	(when (not (null? program))
		(when (and (not (null? (cdar program))))
			(when (symbol? (cadar program))
				(set-label (cadar program) program)))
	(find-labels (cdr program))))

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n")))

; DONE
(define (eval-expr expr)
    (cond 
        ((number? expr) (+ expr 0.0))
        ((symbol? expr) (hash-ref *var-table* expr 0.0))
        ((pair? expr)
            (if (eqv? 'asub (car expr))
                ; then return vector ref 
                (begin 
                    (let ((vector (hash-ref *array-table* (cadr expr))))
                        (vector-ref vector 
                            (exact-round (eval-expr (caddr expr))))))
                ; else apply the function to the map of arguments 
                (let ((func (hash-ref *function-table* (car expr) #f))
                     (opnds (map eval-expr (cdr expr))))
                     (if (not func) nan
                         (apply func opnds)))
            )                
    )  
    (else (not-implemented 'eval-expr expr))))

; DONE   
; create an array with size n and fill the array with 0.0's 
(define (interp-dim args continuation)
    (set-array 
        (cadar args) 
        (make-vector (exact-round (eval-expr (caddar args))) 0.0))
    (interp-program continuation))

; DONE
(define (interp-let args continuation)
	(if (pair? (car args))  
        ; then check to see if its an asub op
		(cond 
			((vector? (hash-ref *array-table* (cadar args)))
             (let ((vector (hash-ref *array-table* (cadar args))))
                (vector-set! vector 
                             (exact-round (eval-expr (caddar args))) 
                             (eval-expr (cadr args)))))) 
        ; else set the specified var value
        (set-var (car args) (eval-expr (cadr args))))
    (interp-program continuation))

; DONE 
(define (interp-goto args continuation)
    (interp-program (hash-ref *label-table* (car args) #f)))

; DONE
(define (interp-if args continuation)
(if ((hash-ref *function-table* (caar args)) 
                                (eval-expr (cadar args)) 
                                (eval-expr (caddar args)))
    ; then go to the label 
    (interp-program (hash-ref *label-table* (cadr args))) 
    ; else continue on 
    (interp-program continuation) ))

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

; interp-input helper function
; Returns the user input 
{define (readnumber)
    (let ((object (read)))
         (cond [(eof-object? object) object]
               [(number? object) (+ object 0.0)]
               [else nan] ))}

; DONE
{define (interp-input args continuation)
    (for-each 
        ; read and store input variable
        (lambda (x) (let ((number (readnumber)))
                (if (eof-object? number)
                    (usage-exit)
                    (set-var x number))))
        ; for each element in this list
        args
    )
    (interp-program continuation)}

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))

(define (interp-program program)
    (when (not (null? program))
        (let ((line (line-stmt (car program)))
            (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                    (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                    (find-labels program)
                       (interp-program program))))))

(main *ARG-LIST*)