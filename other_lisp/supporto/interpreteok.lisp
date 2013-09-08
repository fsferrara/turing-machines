(defun actionstate (squads char)
  (cond
          ( (null squads) nil )
          
          ( (eq (caar squads) char) (cdar squads))
          
          ( T (actionstate (cdr squads) char))
  )
)

(defun searchQuad (quads stato char)
  (cond
          ( (null quads) nil )
          
          ( (eq (caar quads) stato) (actionstate (cdar quads) char) ) 
         
          ( T (searchQuad (cdr quads) stato char))
  )
)

(defun interprete (quads id) 

  (print (append (list (reverse (car id))) (cdr id)) )
  
  (setf acst (searchQuad  quads (cadr id) (caaddr id)) )
  (print acst) ; stampa coppia

  (cond   
          ( (null acst) id )
          
          ( T (cond 
               ((eq (car acst) 'R)
                                        (interprete quads (cond     
                                                    ( (null (cdaddr id)) (list (cons (caaddr id) (car id)) (cadr acst) (cons 'B (cdaddr id))) )
                                                    ( T (list (cons (caaddr id) (car id)) (cadr acst) (cdaddr id)) )
                                                  )
                                        )
                    ) 
                ((eq (car acst) 'L)
                                          (interprete quads 
                                                  (cond
                                                    ( (null (car id)) (list () (cadr acst) (cons 'B (caddr id))) ) 
                                                    ( T (list (cdar id) (cadr acst) (cons (caar id) (caddr id))) )
                                                  )
                                          )
                    )
                    (T (interprete quads (list (car id) (cadr acst) (cons 'B (cdaddr id)) ) )
                    )
                
              )
          )
  )
)

