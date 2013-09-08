; Caro giovanni mettiamo i commenti!!!
; l'input alla macchina sara' costituito da una lista con due sottoliste e lo stato in mezzo.
;	es. (() 1 (I I I B I I I))
;	oppure ((I I I B)3(I I I I))



; WRITECAR: Funzione per scrivere un carattere sul nastro
;		(setf (caaddr ID) s)    ; qui s rappresenta il carattere da scrivere
; Esempi:
;   1. scrive B -> (writecar id 'B)
;   2. scrive I -> (writecar id 'I)
(defun writecar (ID C)
	(setf (caaddr id) C) ; scrivo il carattere C
)



;          DESTRA:
;		(setf (car id )  (cons (caaddr id) (car id)))
;		(setf (caddr id) (cdaddr id))
;		(cond ( (null (caddr id)) (setf (caddr id)  (list 'B))) )  
(defun movedx (ID)
	(setf (car id )  (cons (caaddr id) (car id))) ; PUSH aggiorno sx
	(setf (caddr id) (cdaddr id)) ; POP aggiorno dx
	; se il nastro e' finito aggiungo un B
	(cond ( (null (caddr id)) (setf (caddr id)  (list 'B))) )
)



;          SINISTRA:
;		(eq action 'L)
;		(setf (caddr id) (cons (caar id) (caddr id)))
;		(setf (car id) (cdar id))
;		(cond ( (null (car id)) (setf (car id)  (list 'B))) )
(defun movesx (ID)
	(cond ( (null (car id)) (setf (car id)  (list 'B))) ) ; controllo lista vuota
	(setf (caddr id )  (cons (caar id) (caddr id))) ; PUSH aggiorno dx
	(setf (car id) (cdar id)) ; POP aggiorno sx
	; se il nastro e' finito aggiungo un B
	(cond ( (null (car id)) (setf (car id)  (list 'B))) )
	(print ID)
)



; MDT per la SOMMA
(defun mdtSum (ID)
	(print id)
    (cond 
	
		( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su I
			(writecar id 'B) ; scrivo B
			(mdtSum id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
			(setf (cadr id) 2) ; passo allo stato 2
			(movedx id) ; mi sposto in avanti
			(mdtSum id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su I
			(movedx id) ; mi sposto in avanti
			(mdtSum id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
			(setf (cadr id) 3) ; passo allo stato 3
			(movedx id) ; mi sposto in avanti
			(mdtSum id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
			(writecar id 'B) ; scrivo B
       		(mdtSum id) ; chiamata ricorsiva
		)
		
		(T id) ; fine
      )
)

; MDT per la DIFFERENZA
(defun mdtDif (ID)
	(print id)
    (cond 
	
		( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su I
			(writecar id 'B)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
			(setf (cadr id) 2) ; passo allo stato 2
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su I
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
			(setf (cadr id) 3) ; cambio stato
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'B)) ; STATO 3 - testina su B
			(setf (cadr id) 4) ; cambio stato
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'I)) ; STATO 4 - testina su I
			(writecar id 'B)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'B)) ; STATO 4 - testina su B
			(setf (cadr id) 5) ; cambio stato
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 5) (eq (caaddr id) 'I)) ; STATO 5 - testina su I
			(setf (cadr id) 6) ; cambio stato
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'I)) ; STATO 6 - testina su I
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'B)) ; STATO 6 - testina su B
			(setf (cadr id) 7) ; cambio stato
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'I)) ; STATO 7 - testina su I
			(setf (cadr id) 8) ; cambio stato
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'B)) ; STATO 7 - testina su B
			(setf (cadr id) 9) ; cambio stato
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'I)) ; STATO 8 - testina su I
			(movesx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'B)) ; STATO 8 - testina su B
			(setf (cadr id) 1) ; cambio stato
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 9) (eq (caaddr id) 'I)) ; STATO 9 - testina su I
			(movesx id)
       		(mdtDif id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 9) (eq (caaddr id) 'B)) ; STATO 9 - testina su B
			(movedx id)
			(mdtDif id) ; chiamata ricorsiva
		)
		
		(T id) ; fine
      )
)



; MDT per la DIFFERENZA PROPRIA
(defun mdtDip (ID)
	(print id)
    (cond 
	
		( (and (= (cadr id) 1) (eq (caaddr id) 'I)) ; STATO 1 - testina su I
			(writecar id 'B)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 1) (eq (caaddr id) 'B)) ; STATO 1 - testina su B
			(setf (cadr id) 2) ; passo allo stato 2
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'I)) ; STATO 2 - testina su I
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 2) (eq (caaddr id) 'B)) ; STATO 2 - testina su B
			(setf (cadr id) 3) ; cambio stato
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'I)) ; STATO 3 - testina su I
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 3) (eq (caaddr id) 'B)) ; STATO 3 - testina su B
			(setf (cadr id) 4) ; cambio stato
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'I)) ; STATO 4 - testina su I
			(writecar id 'B)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 4) (eq (caaddr id) 'B)) ; STATO 4 - testina su B
			(setf (cadr id) 5) ; cambio stato
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 5) (eq (caaddr id) 'I)) ; STATO 5 - testina su I
			(setf (cadr id) 6) ; cambio stato
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'I)) ; STATO 6 - testina su I
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 6) (eq (caaddr id) 'B)) ; STATO 6 - testina su B
			(setf (cadr id) 7) ; cambio stato
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'I)) ; STATO 7 - testina su I
			(setf (cadr id) 8) ; cambio stato
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 7) (eq (caaddr id) 'B)) ; STATO 7 - testina su B
			(setf (cadr id) 9) ; cambio stato
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'I)) ; STATO 8 - testina su I
			(movesx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 8) (eq (caaddr id) 'B)) ; STATO 8 - testina su B
			(setf (cadr id) 1) ; cambio stato
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
				
		( (and (= (cadr id) 9) (eq (caaddr id) 'B)) ; STATO 9 - testina su B
			(setf (cadr id) 10) ; cambio stato
			(movedx id)
			(mdtDip id) ; chiamata ricorsiva
		)
		
		( (and (= (cadr id) 10) (eq (caaddr id) 'I)) ; STATO 10 - testina su I
			(setf (cadr id) 9) ; cambio stato
			(writecar id 'B)
       		(mdtDip id) ; chiamata ricorsiva
		)
		
		(T id) ; fine
      )
)

