; Interprete Veloce di MDT

(defun interpr (quads id) 

  (print (append (reverse (car id)) (cdr id)) ) ;stampa id corrente 
 ;(print id)

  (cond   
          ((null quads) id) ; non esiste una quadrupla che gestisce la coppia attiva attuale
          ((eq (caadr id) (caar quads)) ; esiste un azione associata al simbolo letto nello stato corrente
                        (cond 
                             ((eq (cadar quads) 'R) 
                                                  (interpr (caddar quads) ;il prossimo stato è ottenuto spostandosi sulla struttura
                                                                  (cond    ;spostamento a destra
                                                                               ((null (cdadr id)) (list (cons (caadr id)(car id)) (list 'B)))
                                                                               (T (list (cons (caadr id)(car id)) (cdadr id)))
                                                                  ) 
                                                  )
                              ) 
                              ((eq (cadar quads) 'L) 
                                                    (interpr (caddar quads) ;il prossimo stato è ottenuto spostandosi sulla struttura 
                                                                  (cond    ;spostamento a destra
                                                                                ((null (car id)) (list (car id) (cons 'B (cadr id))))
                                                                                (T (list (cdar id) (cons (caar id) (cadr id))))
                                                                  ) 
                                                    )
                              )
                              ;azione di scrittura
                              (T (interpr (caddar quads) ;il prossimo stato è ottenuto spostandosi sulla struttura 
                                  (list (car id) (cons (cadar quads) (cdadr id)) ) )  ; scrittura simbolo presente sulla quadrupla
                              )
                        )
           
           )
           ;il simbolo letto nello stato attuale non corrisponde a quello della quadrupla corrente
          (T (interpr (cdr quads) id)) ; ci si sposta sulla prossima quadrupla dello stato
  )
)
