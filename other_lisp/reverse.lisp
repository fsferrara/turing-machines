; MDT per il REVERSE
(defun mdtRev (ID)
    (print id)
    (cond
	; STATO 1
		( (and (= (cadr id) 1) (eq (caaddr id) 'f))
			(mdtRev (list (car id) '2 (cons 'B (cdaddr id)) ) )
		)
		( (and (= (cadr id) 1) (eq (caaddr id) 'd))
			(mdtRev (list (car id) '6 (cons 'B (cdaddr id)) ) )
		)
		( (and (= (cadr id) 1) (eq (caaddr id) 'e))
			(mdtRev (list (car id) '9 (cons 'B (cdaddr id)) ) )
		)

		( (and (= (cadr id) 1) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '21 (cons 'B (cdaddr id)) ) )
		)

	; STATO 2
		( (and (= (cadr id) 2) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)

	; STATO 3
		( (and (= (cadr id) 3) (eq (caaddr id) 'd))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 3) (eq (caaddr id) 'e))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 3) (eq (caaddr id) 'f))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '3 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '3 (cdaddr id))) )
			)
		)

		( (and (= (cadr id) 3) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '4 (cons 'f (cdaddr id)) ) )
		)

	; STATO 4
		( (and (= (cadr id) 4) (eq (caaddr id) 'f))
			(cond
			   ( (null (car id)) (mdtRev (list () '5 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '5 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 5
		( (and (= (cadr id) 5) (eq (caaddr id) 'e))
			(mdtRev (list (car id) '12 (cons 'B (cdaddr id)) ) )
		)
		( (and (= (cadr id) 5) (eq (caaddr id) 'd))
			(mdtRev (list (car id) '15 (cons 'B (cdaddr id)) ) )
		)
		( (and (= (cadr id) 5) (eq (caaddr id) 'f))
			(mdtRev (list (car id) '18 (cons 'B (cdaddr id)) ) )
		)

		( (and (= (cadr id) 5) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '21 (cons 'B (cdaddr id)) ) )
		)

	; STATO 6
		( (and (= (cadr id) 6) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '7 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '7 (cdaddr id))) )
			)
		)

	; STATO 7
		( (and (= (cadr id) 7) (eq (caaddr id) 'd))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '7 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '7 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 7) (eq (caaddr id) 'e))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '7 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '7 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 7) (eq (caaddr id) 'f))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '7 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '7 (cdaddr id))) )
			)
		)

		( (and (= (cadr id) 7) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '8 (cons 'd (cdaddr id)) ) )
		)

	; STATO 8
		( (and (= (cadr id) 8) (eq (caaddr id) 'd))
			(cond
			   ( (null (car id)) (mdtRev (list () '5 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '5 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 9
		( (and (= (cadr id) 9) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '10 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '10 (cdaddr id))) )
			)
		)

	; STATO 10
		( (and (= (cadr id) 10) (eq (caaddr id) 'd))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '10 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '10 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 10) (eq (caaddr id) 'e))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '10 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '10 (cdaddr id))) )
			)
		)
		( (and (= (cadr id) 10) (eq (caaddr id) 'f))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '10 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '10 (cdaddr id))) )
			)
		)

		( (and (= (cadr id) 10) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '11 (cons 'e (cdaddr id)) ) )
		)

	; STATO 11
		( (and (= (cadr id) 11) (eq (caaddr id) 'e))
			(cond
			   ( (null (car id)) (mdtRev (list () '5 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '5 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 12
		( (and (= (cadr id) 12) (eq (caaddr id) 'B))
			(cond
			   ( (null (car id)) (mdtRev (list () '13 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '13 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 13
		( (and (= (cadr id) 13) (eq (caaddr id) 'd))
			(cond
			   ( (null (car id)) (mdtRev (list () '13 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '13 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 13) (eq (caaddr id) 'e))
			(cond
			   ( (null (car id)) (mdtRev (list () '13 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '13 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 13) (eq (caaddr id) 'f))
			(cond
			   ( (null (car id)) (mdtRev (list () '13 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '13 (cons (caar id) (caddr id)))) )
			)
		)

		( (and (= (cadr id) 13) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '14 (cons 'e (cdaddr id)) ) )
		)

	; STATO 14
		( (and (= (cadr id) 14) (eq (caaddr id) 'e))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
			)
		)

	; STATO 15
		( (and (= (cadr id) 15) (eq (caaddr id) 'B))
			(cond
			   ( (null (car id)) (mdtRev (list () '16 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '16 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 16
		( (and (= (cadr id) 16) (eq (caaddr id) 'd))
			(cond
			   ( (null (car id)) (mdtRev (list () '16 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '16 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 16) (eq (caaddr id) 'e))
			(cond
			   ( (null (car id)) (mdtRev (list () '16 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '16 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 16) (eq (caaddr id) 'f))
			(cond
			   ( (null (car id)) (mdtRev (list () '16 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '16 (cons (caar id) (caddr id)))) )
			)
		)

		( (and (= (cadr id) 16) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '17 (cons 'd (cdaddr id)) ) )
		)

	; STATO 17
		( (and (= (cadr id) 17) (eq (caaddr id) 'd))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
			)
		)

	; STATO 18
		( (and (= (cadr id) 18) (eq (caaddr id) 'B))
			(cond
			   ( (null (car id)) (mdtRev (list () '19 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '19 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 19
		( (and (= (cadr id) 19) (eq (caaddr id) 'd))
			(cond
			   ( (null (car id)) (mdtRev (list () '19 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '19 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 19) (eq (caaddr id) 'e))
			(cond
			   ( (null (car id)) (mdtRev (list () '19 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '19 (cons (caar id) (caddr id)))) )
			)
		)
		( (and (= (cadr id) 19) (eq (caaddr id) 'f))
			(cond
			   ( (null (car id)) (mdtRev (list () '19 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '19 (cons (caar id) (caddr id)))) )
			)
		)

		( (and (= (cadr id) 19) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '20 (cons 'f (cdaddr id)) ) )
		)

	; STATO 20
		( (and (= (cadr id) 20) (eq (caaddr id) 'f))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
			)
		)

	; STATO 21
		( (and (= (cadr id) 21) (eq (caaddr id) 'B))
			(cond
			   ( (null (car id)) (mdtRev (list () '22 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '22 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 22 (FINALE)
		( (and (= (cadr id) 22) (eq (caaddr id) 'd))
			(mdtRev (list (car id) '26 (cons 'B (cdaddr id)) ) )
		)

		( (and (= (cadr id) 22) (eq (caaddr id) 'e))
			(mdtRev (list (car id) '23 (cons 'B (cdaddr id)) ) )
		)

		( (and (= (cadr id) 22) (eq (caaddr id) 'f))
			(mdtRev (list (car id) '29 (cons 'B (cdaddr id)) ) )
		)

	; STATO 23
		( (and (= (cadr id) 23) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '24 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '24 (cdaddr id))) )
			)
		)

	; STATO 24
		( (and (= (cadr id) 24) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '25 (cons 'e (cdaddr id)) ) )
		)

	; STATO 25
		( (and (= (cadr id) 25) (eq (caaddr id) 'e))
			(cond
			   ( (null (car id)) (mdtRev (list () '21 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '21 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 26
		( (and (= (cadr id) 26) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '27 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '27 (cdaddr id))) )
			)
		)

	; STATO 27
		( (and (= (cadr id) 27) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '28 (cons 'd (cdaddr id)) ) )
		)

	; STATO 28
		( (and (= (cadr id) 28) (eq (caaddr id) 'd))
			(cond
			   ( (null (car id)) (mdtRev (list () '21 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '21 (cons (caar id) (caddr id)))) )
			)
		)

	; STATO 29
		( (and (= (cadr id) 29) (eq (caaddr id) 'B))
			(cond
			   ( (null (cdaddr id)) (mdtRev (list (cons (caaddr id) (car id)) '30 (cons 'B (cdaddr id)))) )
			   ( T (mdtRev (list (cons (caaddr id) (car id)) '30 (cdaddr id))) )
			)
		)

	; STATO 30
		( (and (= (cadr id) 30) (eq (caaddr id) 'B))
			(mdtRev (list (car id) '31 (cons 'f (cdaddr id)) ) )
		)

	; STATO 31
		( (and (= (cadr id) 31) (eq (caaddr id) 'f))
			(cond
			   ( (null (car id)) (mdtRev (list () '21 (cons 'B (caddr id)))) )
			   ( T (mdtRev (list (cdar id) '21 (cons (caar id) (caddr id)))) )
			)
		)


		(T id) ; fine
      )
)

