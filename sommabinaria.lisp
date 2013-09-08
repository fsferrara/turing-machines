; MDT per la SOMMA BINARIA
(defun mdtSub (ID)
	(print (append (list (reverse (car id))) (cdr id)) )
    (cond 

( (and (= (cadr id) 1) (eq (caaddr id) 'O)) ; STATO 1 - testina su O I Z U D
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
	)
)
( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su O I Z U D
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
	)
)
( (and (= (cadr id) 1) (eq (caaddr id) 'Z)) ; STATO 1 - testina su O I Z U D
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
	)
)
( (and (= (cadr id) 1) (eq (caaddr id) 'U)) ; STATO 1 - testina su O I Z U D
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
	)
)
( (and (= (cadr id) 1) (eq (caaddr id) 'D)) ; STATO 1 - testina su O I Z U D
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '1 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '1 (cdaddr id))) )
	)
)

( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
	)
)

( (and (= (cadr id) 2) (eq (caaddr id) 'O)) ; STATO 2 - testina su O I
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
	)
)
( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su O I
	(cond
	   ( (null (cdaddr id)) (mdtSub (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
	   ( T (mdtSub (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
	)
)

( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '3 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '3 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
	(mdtSub (list (car id) '4 (cons 'B (cdaddr id)) ) )
)

( (and (= (cadr id) 3) (eq (caaddr id) 'O)) ; STATO 3 - testina su O
	(mdtSub (list (car id) '7 (cons 'B (cdaddr id)) ) )
)

( (and (= (cadr id) 3) (eq (caaddr id) 'B)) ; STATO 3 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '10 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '10 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 4) (eq (caaddr id) 'B)) ; STATO 4 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '5 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '5 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 5) (eq (caaddr id) 'O)) ; STATO 5 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '5 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '5 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 5) (eq (caaddr id) 'I)) ; STATO 5 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '5 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '5 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 5) (eq (caaddr id) 'B)) ; STATO 5 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '6 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '6 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 6) (eq (caaddr id) 'Z)) ; STATO 6 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '6 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '6 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 6) (eq (caaddr id) 'U)) ; STATO 6 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '6 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '6 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 6) (eq (caaddr id) 'D)) ; STATO 6 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '6 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '6 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 6) (eq (caaddr id) 'I)) ; STATO 6 - testina su I
	(mdtSub (list (car id) '1 (cons 'D (cdaddr id)) ) )
)

( (and (= (cadr id) 6) (eq (caaddr id) 'O)) ; STATO 6 - testina su O B
	(mdtSub (list (car id) '1 (cons 'U (cdaddr id)) ) )
)
( (and (= (cadr id) 6) (eq (caaddr id) 'B)) ; STATO 6 - testina su O B
	(mdtSub (list (car id) '1 (cons 'U (cdaddr id)) ) )
)

( (and (= (cadr id) 7) (eq (caaddr id) 'B)) ; STATO 7 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '8 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '8 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 8) (eq (caaddr id) 'O)) ; STATO 8 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '8 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '8 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 8) (eq (caaddr id) 'I)) ; STATO 8 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '8 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '8 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 8) (eq (caaddr id) 'B)) ; STATO 8 - testina su B
	(cond
	   ( (null (car id)) (mdtSub (list () '9 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '9 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 9) (eq (caaddr id) 'Z)) ; STATO 9 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '9 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '9 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 9) (eq (caaddr id) 'U)) ; STATO 9 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '9 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '9 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 9) (eq (caaddr id) 'D)) ; STATO 9 - testina su Z U D
	(cond
	   ( (null (car id)) (mdtSub (list () '9 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '9 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 9) (eq (caaddr id) 'I)) ; STATO 9 - testina su I
	(mdtSub (list (car id) '1 (cons 'U (cdaddr id)) ) )
)

( (and (= (cadr id) 9) (eq (caaddr id) 'O)) ; STATO 9 - testina su O B
	(mdtSub (list (car id) '1 (cons 'Z (cdaddr id)) ) )
)
( (and (= (cadr id) 9) (eq (caaddr id) 'B)) ; STATO 9 - testina su O B
	(mdtSub (list (car id) '1 (cons 'Z (cdaddr id)) ) )
)

( (and (= (cadr id) 10) (eq (caaddr id) 'Z)) ; STATO 10 - testina su Z
	(mdtSub (list (car id) '10 (cons 'O (cdaddr id)) ) )
)

( (and (= (cadr id) 10) (eq (caaddr id) 'U)) ; STATO 10 - testina su U
	(mdtSub (list (car id) '10 (cons 'I (cdaddr id)) ) )
)

( (and (= (cadr id) 10) (eq (caaddr id) 'O)) ; STATO 10 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '10 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '10 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 10) (eq (caaddr id) 'I)) ; STATO 10 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '10 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '10 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 10) (eq (caaddr id) 'D)) ; STATO 10 - testina su D
	(mdtSub (list (car id) '11 (cons 'O (cdaddr id)) ) )
)

( (and (= (cadr id) 11) (eq (caaddr id) 'O)) ; STATO 11 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '12 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '12 (cons (caar id) (caddr id)))) )
	)
)
( (and (= (cadr id) 11) (eq (caaddr id) 'I)) ; STATO 11 - testina su O I
	(cond
	   ( (null (car id)) (mdtSub (list () '12 (cons 'B (caddr id)))) )
	   ( T (mdtSub (list (cdar id) '12 (cons (caar id) (caddr id)))) )
	)
)

( (and (= (cadr id) 12) (eq (caaddr id) 'B)) ; STATO 12 - testina su B O Z
	(mdtSub (list (car id) '10 (cons 'I (cdaddr id)) ) )
)
( (and (= (cadr id) 12) (eq (caaddr id) 'O)) ; STATO 12 - testina su B O Z
	(mdtSub (list (car id) '10 (cons 'I (cdaddr id)) ) )
)
( (and (= (cadr id) 12) (eq (caaddr id) 'Z)) ; STATO 12 - testina su B O Z
	(mdtSub (list (car id) '10 (cons 'I (cdaddr id)) ) )
)

( (and (= (cadr id) 12) (eq (caaddr id) 'I)) ; STATO 12 - testina su I U
	(mdtSub (list (car id) '11 (cons 'O (cdaddr id)) ) )
)
( (and (= (cadr id) 12) (eq (caaddr id) 'U)) ; STATO 12 - testina su I U
	(mdtSub (list (car id) '11 (cons 'O (cdaddr id)) ) )
)

( (and (= (cadr id) 12) (eq (caaddr id) 'D)) ; STATO 12 - testina su D
	(mdtSub (list (car id) '11 (cons 'I (cdaddr id)) ) )
)

(T id) ; fine

      )
)
