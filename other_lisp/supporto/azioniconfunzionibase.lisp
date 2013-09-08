l'input alla macchina sarà costituito da una lista con due sottoliste e lo stato in mezzo.
	es. (() 1 (I I I B I I I))
	oppure ((I I I B)3(I I I I))



=====[NON DISTRUTTIVE]=====
SCRITTURA:
			(mdtXxx (list (car id) '1 (cons 'B (cdaddr id)) ) )

R DESTRA:
			(cond
			   ( (null (cdaddr id)) (mdtXxx (list (cons (caaddr id) (car id)) '2 (cons 'B (cdaddr id)))) )
			   ( T (mdtXxx (list (cons (caaddr id) (car id)) '2 (cdaddr id))) )
			)

L SINISTRA:
			(cond
			   ( (null (car id)) (mdtXxx (list () '1 (cons 'B (caddr id)))) )
			   ( T (mdtXxx (list (cdar id) '1 (cons (caar id) (caddr id)))) )
			)

CAMBIO STATO:
			(mdtXxx (list (car id) '2 (caddr id) ) )
  


===[FUNZIONI DISTRUTTIVE]===
SCRITTURA:
  (setf (caaddr ID) 's)    ; qui s rappresenta il carattere da scrivere

R DESTRA:
  (setf (car id )  (cons (caaddr id) (car id)))
  (setf (caddr id) (cdaddr id))
  (cond ( (null (caddr id)) (setf (caddr id)  (list 'B))) )

L SINISTRA:
  (cond ( (null (car id)) (setf (car id)  (list 'B))))
  (setf (caddr id) (cons (caar id) (caddr id)))
  (setf (car id) (cdar id))  

CAMBIO STATO:
  (setf (cadr id) 2)


