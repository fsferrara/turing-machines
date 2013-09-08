(defun fatt(a)
	(cond 
  		((eq a 0) 1)
		(t (* a (fatt (- a 1)) ))
)

)

(defun spiana(L)
	(cond
		((null L) nil)
		((atom (CAR L)) (append (list (CAR L)) (spiana (CDR L))))
		(t (append (spiana (CAR L)) (spiana (CDR L))))
	)
)

(defun lista(v1 &rest L)
	(print v1)
	(cond
		((null v1) nil)
		(t (lista (CAR L) (CDR L)))
	)
)

