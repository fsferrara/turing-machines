; INTERPRETE DI MDT

;funzione per la ricerca della coppia azione/nuovo-stato tra le quadruple dello stato corrente
;input : 
;         squads  -> quadruple associate allo stato corrente
;         char    -> simbolo sotto la testina
;output : coppia (X Y) dove X è l'azione e Y il nuovo stato
(defun actionstate (squads char)
 ;(print squads)
  (cond
          ( (null squads) nil ) ;non è stata trovata nessuna quadrupla che gestisce il simbolo letto
          
          ( (eq (caar squads) char) (cdar squads)) ; il simbolo è gestito dalla quadrupla corrente
          
          ( T (actionstate (cdr squads) char)) ; ricerca sulla prossima quadrupla
  )
)


;funzione per la ricerca della coppia azione/nuovo-stato tra le quadruple della macchina
;input : 
;         quads   -> insieme di quadruple della macchina
;         stato   -> stato corrente
;         char    -> simbolo sotto la testina
;output : coppia (X Y) dove X è l'azione e Y il nuovo stato
(defun searchQuad (quads stato char)
  (cond
          ( (null quads) nil ) ; lo stato corrente non appartine a quelli gastiti dalle quadruple (errore)
          
          ( (eq (caar quads) stato) (actionstate (cdar quads) char) ) ; trovato il set di quadruple legate allo stato corrente
                                                                      ; si richiama la funzione actionstate per la ricerca della coppia (azione nuovo-stato)          
          ( T (searchQuad (cdr quads) stato char)) ; ricerca sul prossimo set di quadruple
  )
)

; funzione lisp interprete di mdt
;input : 
;         quads   -> insieme di quadruple della macchina della forma:
;                             ( (nomeStato (carattere azione nuovo-stato )(carattere azione nuovo-stato). ..))
;                               (nomeStato (carattere azione nuovo-stato )(carattere azione nuovo-stato). ..))
;                                .
;                                .
;                                .
;                              )
;         id  -> id data in pasto alla machina della forma:
;                                  ((simboli a sinistra sul nastro in ordine inverso) nomeStato (simboli a destra sul nastro))
;output : coppia (X Y) dove X è l'azione e Y il nuovo stato
(defun interprete (quads id) 

  (print (append (list (reverse (car id))) (cdr id)) ) ;stampa id corrente 
  
  (setf acst (searchQuad  quads (cadr id) (caaddr id)) ) ; ricerca della coppia (azione nuovo-stato)
  (print acst) ; stampa coppia

  (cond   
  ( (null acst) id ) ; non esiste una quadrupla che gestisce la coppia attiva attuale
  
  ( T (cond 
       ((eq (car acst) 'R)  ; spotamento testina a destra
                (interprete quads (cond     
                    ( (null (cdaddr id)) (list (cons (caaddr id) (car id)) (cadr acst) (cons 'B (cdaddr id))) )
                    ( T (list (cons (caaddr id) (car id)) (cadr acst) (cdaddr id)) )
                  )
                )
            ) 
        ((eq (car acst) 'L) ; spotamento testina a sinistra
                  (interprete quads 
                  (cond
                    ( (null (car id)) (list () (cadr acst) (cons 'B (caddr id))) ) 
                    ( T (list (cdar id) (cadr acst) (cons (caar id) (caddr id))) )
                  )
                  )
            )
            ;azione di scrittura
            (T (interprete quads (list (car id) (cadr acst) (cons 'B (cdaddr id)) ) )  ; scrittura simbolo presente sulla quadrupla
            )
        
      )
  )
  )
)
