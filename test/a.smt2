(declare-fun f (Int) Int)
(declare-fun a () Int) ; a is a constant
(define-fun q ((i Int)) Int (* 2 i))
(declare-const b Int) ; syntax sugar for (declare-fun b () Int)
(assert (> a 20))
(assert (> b a))
(assert (= (f 10) 1))
(check-sat)
(get-model)
