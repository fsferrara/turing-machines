; COMPILATORE DI MDT

(defun quadInstr (qi sj xi ql nome)
    
  (cond ( (eq xi 'R) `(( (and (= (cadr id) ,qi) (eq (caaddr id) ',sj))(cond
			   ( (null (cdaddr id)) (,nome (list (cons (caaddr id) (car id)) ',ql (cons 'B (cdaddr id)))) )
			   ( T (,nome (list (cons (caaddr id) (car id)) ',ql (cdaddr id))) )
			)))
         )
        
        ( (eq xi 'L) `(( (and (= (cadr id) ,qi) (eq (caaddr id) ',sj))(cond
			   ( (null (car id)) (,nome (list () ',ql (cons 'B (caddr id)))) )
			   ( T (,nome (list (cdar id) ',ql (cons (caar id) (caddr id)))) )
			)))
         )
     (T `(( (and (= (cadr id) ,qi) (eq (caaddr id) ',sj))(,nome (list (car id) ',ql (cons ',xi (cdaddr id)) ) ))))
    )
)

(defun TbodyState (squads Tname State)
  (cond
    ( (null squads) nil )
    ( T (append 
         (quadInstr State (caar squads) (cadar squads) (caddar squads) Tname)
         (TbodyState (cdr squads) Tname State)
         )
     )
    )
  )



(defun Tbody (quads Tname)
  (cond
    ( (null quads) nil ) 
    ( T (append
         (TbodyState (cdar quads) Tname ( caar quads)) 
         (Tbody (cdr quads) Tname)
         )
     )
    )
  )


(defun TuringCompile (quads nome)
  (cond 
          ( (null quads) nil)
          ((null nome) nil)
          (T `(defun ,nome (ID) (print (append (list (reverse (car id))) (cdr id)) ) (cond ,@(Tbody quads nome) (T id) ) ) )
  ) 
)
