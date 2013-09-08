; MDT per la SOMMA
(defun mdtSum (ID)
	(print id)
    (cond 
	
		( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su I
			(mdtSum (list (car id) '1 (cons 'B (cdaddr id)) ) )
		)
		
		( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtSum (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
			   ( T (mdtSum (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su I
			(cond
			   ( (null (cdaddr id)) (mdtSum (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
			   ( T (mdtSum (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtSum (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtSum (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
			(mdtSum (list (car id) '1 (cons 'B (cdaddr id)) ))
		)
		
		(T id) ; fine
      )
)
