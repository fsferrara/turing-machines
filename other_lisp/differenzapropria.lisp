; MDT per la DIFFERENZA PROPRIA
(defun mdtDip (ID)
    (print id)
    (cond 
	
		( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su I
			(mdtDip (list (car id) '1 (cons 'B (cdaddr id)) ) )
		)
		
		( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su I
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'B)) ; STATO 3 - testina su B
			(cond
			   ( (null (car id)) (mdtDip (list () '4 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '4 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'I)) ; STATO 4 - testina su I
			(mdtDip (list (car id) '4 (cons 'B (cdaddr id)) ) )
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'B)) ; STATO 4 - testina su B
			(cond
			   ( (null (car id)) (mdtDip (list () '5 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '5 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 5) (eq (caaddr id) 'I)) ; STATO 5 - testina su I
			(cond
			   ( (null (car id)) (mdtDip (list () '6 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '6 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'I)) ; STATO 6 - testina su I
			(cond
			   ( (null (car id)) (mdtDip (list () '6 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '6 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'B)) ; STATO 6 - testina su B
			(cond
			   ( (null (car id)) (mdtDip (list () '7 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '7 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'I)) ; STATO 7 - testina su I
			(cond
			   ( (null (car id)) (mdtDip (list () '8 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '8 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'B)) ; STATO 7 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '9 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '9 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'I)) ; STATO 8 - testina su I
			(cond
			   ( (null (car id)) (mdtDip (list () '8 (cons 'B (caddr id)))) )
			   ( T (mdtDip (list (cdar id) '8 (cons (caar id) (caddr id)))) )
			)
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'B)) ; STATO 8 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
			)
		)
		
		( (and (= (cadr id) 9) (eq (caaddr id) 'B)) ; STATO 9 - testina su B
			(cond
			   ( (null (cdaddr id)) (mdtDip (list (cons (caaddr id) (car id)) '10 (cons 'B (cdaddr id)))) )
			   ( T (mdtDip (list (cons (caaddr id) (car id)) '10 (cdaddr id))) )
			)
		)

		( (and (= (cadr id) 9) (eq (caaddr id) 'I)) ; STATO 10 - testina su I
			(mdtDip (list (car id) '9 (cons 'B (cdaddr id)) ) )
		)
		
		(T id) ; fine
      )
)

