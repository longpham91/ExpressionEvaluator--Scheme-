#lang racket

;;Group: Long Pham, Nazir Tokhi, Matt Schramm, Jeromy Sisk, David Chew
;;CSC171-2: Scheme Programming Language
;;Professor William Turner
;;Final Project

;;In this project we intend to create an expression evaluator, using the infix->prefix
;;algorithm. The evaluator will take an expression written in pseudo form, reread it in 
;;recursive form (Scheme format), and then evaluate the expression's value.

;;First, we create a list of operators, on top of built-in operators like +, -, *, /, etc

;;compare two numbers
(define (comp x y)
  (cond ((< x y) y)
        ((>= x y) x)
        )
  )

;;sine
(define (xsin x y)
  (* x (sin y)))

;;cosine
(define (xcos x y)
  (* x (cos y)))

;;tangent
(define (xtan x y)
  (* x (tan y)))

;;arcsin
(define (xasin x y)
  (* x (asin y)))

;;arccos
(define (xacos x y)
  (* x (acos y)))

;;arctan
(define (xatan x y)
  (* x (atan y)))

;;logarithm (base x)
(define (xlog x y)
  (/ (log y) (log x)))

;;exponential
(define (xexp x y)
  (* x (exp y)))

;;take derivative of a function
(define (derivat exp var)
  (cond
    ((constant? exp) 0)
    ((variable? exp)
     (if (same-var? exp var)
         1 0))
    ((sum? exp) (make-sum (derivat (term1 exp) var)
                          (derivat (term2 exp) var)))
    ((product? exp)
     (make-sum (make-product (derivat (fact1 exp) var) (fact2 exp))
               (make-product (fact1 exp) (derivat (fact2 exp) var))))
    ))

(define constant? number?)
(define variable? symbol?)
(define same-var? eq?)
(define (sum? exp)
  (and (pair? exp) (eq? '+ (cadr exp))))
(define (make-sum a b)
  (cond ((eq? a 0) b)
        ((eq? b 0) a)
        (else (list a '+ b))))
(define term1 car)
(define term2 caddr)
(define (product? exp)
  (and (pair? exp) (eq? '* (cadr exp))))
(define (make-product a b)
  (cond ((eq? a 0) 0)
        ((eq? b 0) 0)
        ((eq? a 1) b)
        ((eq? b 1) a)
        (else (list a '* b))))
(define fact1 car)
(define fact2 caddr)

;Testing
(derivat '((x * x) + ((b * x) + c))  'x)

;;converting the pseudo form to recursive form so that Scheme can comprehend
(define infix->prefix
  (lambda (inputlist)
    ;    (display "Called infix->prefix on ")
    ;    (display inputlist)
    ;    (display "\n")
    (define heirarchy
      (lambda (operator)
        (cond
          ((equal? operator '+) 1)
          ((equal? operator '-) 1)
          ((equal? operator '*) 2)
          ((equal? operator '/) 2)
          ((equal? operator '^) 3)
          ((equal? operator 'right-paren) 0)
          ((equal? operator 'left-paren) 0)
          ((equal? operator 'remainder) 4)
          ((equal? operator 'comp) 2)
          ((equal? operator 'xexp) 1)
          ((equal? operator 'xsin) 3)
          ((equal? operator 'xcos) 3)
          ((equal? operator 'xtan) 3)
          ((equal? operator 'xasin) 3)
          ((equal? operator 'xacos) 3)
          ((equal? operator 'xatan) 3)
          ((equal? operator 'xlog) 3)
          ((equal? operator 'derivat) 4)  
          (else 1000)
          )
        )
      )
    (let helper ((infix inputlist) (operands '()) (operators '()))
      (if (null? infix)
          ;                    (begin (display "Empty infix list, operands = ")
          ;                           (display operands)
          ;                           (display " and operators = ")
          ;                           (display operators)
          ;                           (display "\n")
          ;                           )
          (if (null? operators)
              (car operands)
              (helper infix 
                      (cons (list (car operators)
                                  (cadr operands)
                                  (car operands)
                                  )
                            (cddr operands)
                            )
                      (cdr operators)
                      )
              )
          (let ((token (car infix)))
            ;                        (display "token = ")
            ;                        (display token)
            ;                        (display "\n")
            (cond
              ((number? token) (helper (cdr infix) (cons token operands) operators))
              ((equal? token 'left-paren) (helper (cdr infix) operands (cons token operators)))
              ((null? operators) (helper (cdr infix) operands (cons token operators)))
              ((> (heirarchy token) (heirarchy (car operators))) (helper (cdr infix) operands (cons token operators)))
              ((equal? token 'right-paren) (if (equal? (car operators) 'left-paren)
                                               (helper (cdr infix) operands (cdr operators)) ;; Throw both parentheses away
                                               (helper infix ;; leave right-paren on infix!
                                                       (cons (list (car operators)
                                                                   (cadr operands)
                                                                   (car operands)
                                                                   )
                                                             (cddr operands)
                                                             )
                                                       (cdr operators)
                                                       )
                                               )
                                           )
              ((<= (heirarchy token) (heirarchy (car operators))) (helper infix ;; leave token on infix
                                                                          (cons (list (car operators)
                                                                                      (cadr operands)
                                                                                      (car operands)
                                                                                      )
                                                                                (cddr operands)
                                                                                )
                                                                          (cdr operators)
                                                                          )
                                                                  )
              (else
               (begin (display "Stopped early, Infix = ")
                      (display infix)
                      (display ", operands = ")
                      (display operands)
                      (display " and operators = ")
                      (display operators)
                      (display "\n")
                      )
               )
              )
            )
          )
      )
    )
  )

;;create "tokens" for the operators. For example, "sin" stands for the function (xsin x y) defined above.
(define make-math
  (lambda (char)
    (cond ((equal? char '+) +)
          ((equal? char '-) -)
          ((equal? char '*) *)
          ((equal? char '/) /)
          ((equal? char '^) expt)
          ((equal? char '%) remainder)
          ((equal? char '?) comp)
          ((equal? char '~) xexp)
          ((equal? char 'sin) xsin)
          ((equal? char 'cos) xcos)
          ((equal? char 'tan) xtan)
          ((equal? char 'arcsin) xasin)
          ((equal? char 'arccos) xacos)
          ((equal? char 'arctan) xatan)
          ((equal? char 'log) xlog)
          ((equal? char 'deriv) derivat)  
          (else (lambda (x) x)))))

;;individually takes each piece of the list and recombines it in the previous code
(define atom?
  (lambda (thing)
    (not (or (null? thing) (list? thing)))))

;;recombines the elements and turns the procedure into a working built-in notation
(define prefix-char-math
  (lambda (expr)
    (if (atom? expr)
        expr
        (apply (make-math (car expr))
               (map prefix-char-math (cdr expr))))))

;;simplify the code
(define math
  (lambda (lst)
    (if (not (equal? '() "quit"))
        (begin
          (display (prefix-char-math (infix->prefix lst)))
          (display "\n")
          )
        (display "Solved!")
        )
    )
  )

"Testing infix->prefix..."
;(prefix-char-math '(+ (+ 3 (* 5 4)) 7))
;(prefix-char-math (infix->prefix '(3 + 5 * 7 - 2)))
;(prefix-char-math (infix->prefix '(10 log 1)))
(math '(3 + 5 * 7 - 2))
(math '(1 sin 5))
(math '(10 log 5))
(math '(1 ~ 2))
