;*****************************************************************
;* This file contains various training routines written for
;* GNU Common Lisp, available at http://www.gnu.org/software/gcl
;* 
;* Author: Salvatore Iengo <salvatoreiengo@hotmail.com>
;*         computer science student at
;*         University of Napoli Federico II
;*
;* The content of this file is available at:
;*    http://aioros.sourceforge.net/turing.lsp         
;*****************************************************************

;Turing machine simulation (adder & reverser)
;
;
;The following specs are adopted from all the routines:
;
;      - 'ID' denotes a Turing machine istantaneous description
;        (TMID). That's the form:
;                   `(listA consC listB)
;        where 'listA' is the list of the symbols preceding
;        the machine head, 'listB' the list of the symbols 
;        at the right of the head and 'consC' is the dotted
;        pair concerning the TM status and the current symbol
;        on the tape.
;
;        .e.g. the lisp 'ID':
;              `(B 1 0 0),(cons 'q0 'B)(B 1 1 1)
;        encodes the turing machine TMID:
;               0 0 1 B q0 B B 1 1 1
;        please note the reverse order of the first part.
;
;      - 'quad' denotes a Turing machine quadruple by the use
;        of a list.
;
;        .e.g. the quadruple 'quad':
;              '(q0 1 1 q0)
;        encodes the TM quadr.:
;               q0 1 1 q0 
          
;*****************************************************************
;* Given a non-moving quadruple and a ID
;* yelds the next ID 
;*****************************************************************
(defun newreplace (ID quad)
  (let*(
        (prev(first ID))
        (curr(second ID))
        (next(third ID))
        (qi(first quad))
        (si(second quad))
        (sj(third quad))
        (qj(fourth quad))
        )
        (if (equal (cons qi si) curr)
           (list prev (cons qj sj) next)
           nil)))
           
;*****************************************************************
;* Given a right-moving quadruple and a ID
;* yelds the next ID 
;*****************************************************************
(defun right (ID quad)
  (let*(
        (prev(first ID))
        (curr(second ID))
        (next(third ID))
        (qi(first quad))
        (si(second quad))
        (sj(third quad))
        (qj(fourth quad))
        )
        (if (equal (cons qi si) curr)
           (list (if prev (append (list(cdr curr)) prev) (list(cdr curr)))
                 (cons qj (if (car next) (car next) 'B))
                 (cdr next))
           nil)))

;*****************************************************************
;* Given a left-moving quadruple and a ID
;* yelds the next ID 
;*****************************************************************
(defun left (ID quad)
  (let*(
        (prev(first ID))
        (curr(second ID))
        (next(third ID))
        (qi(first quad))
        (si(second quad))
        (sj(third quad))
        (qj(fourth quad))
        )
        (if (equal (cons qi si) curr)
           (list (cdr prev)
                 (cons qj (if (car prev) (car prev) 'B))
                 (if next (append (list(cdr curr)) next) (list(cdr curr))))
           nil)))
           
;*****************************************************************
;* Given a quadruple and a ID
;* yelds the next ID            
;*****************************************************************
(defun transition (ID quad)
   (cond((equal (third quad) 'R) (right ID quad))
        ((equal (third quad) 'L) (left  ID quad))
        (t (newreplace ID quad))))

;*****************************************************************
;* Turing Machine FULL ADDER, iterative implementation
;*
;* TM info:
;*    Alphabet: {0 1 B $ Z E}
;*    Initial status: Q0
;*
;* Parameters:
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (adderTM `((),(cons 'Q0 'B)(1 1 1 1 B 1)))
;* 
;*      ((B) (Q53 . B) (1 0 0 0 0 B))
;*    >
;*****************************************************************
(defun adderTM (ID &optional trac)
   (do*((ID ID
        (or
        (transition ID '(q0 0 L q1))
        (transition ID '(q0 1 L q1))
        (transition ID '(q0 B $ q20))
        (transition ID '(q20 $ R q2))
        (transition ID '(q0 $ $ q0))
        (transition ID '(q0 Z Z q0))
        (transition ID '(q0 E E q0))
        (transition ID '(q1 0 0 q1))
        (transition ID '(q1 1 1 q1))
        (transition ID '(q1 B $ q21))
        (transition ID '(q21 $ R q2))
        (transition ID '(q1 $ $ q1))
        (transition ID '(q1 Z Z q1))
        (transition ID '(q1 E E q1))
        (transition ID '(q2 0 R q3))
        (transition ID '(q2 1 R q3))
        (transition ID '(q2 B R q2))
        (transition ID '(q2 $ $ q2))
        (transition ID '(q2 Z Z q2))
        (transition ID '(q2 E E q2))
        (transition ID '(q3 0 R q3))
        (transition ID '(q3 1 R q3))
        (transition ID '(q3 B L q4))
        (transition ID '(q3 $ $ q3))
        (transition ID '(q3 Z Z q3))
        (transition ID '(q3 E E q3))
        (transition ID '(q4 0 B q22))
        (transition ID '(q22 B R q6))
        (transition ID '(q4 1 B q23))
        (transition ID '(q23 B R q5))
        (transition ID '(q4 B B q4))
        (transition ID '(q4 $ $ q4))
        (transition ID '(q4 Z Z q4))
        (transition ID '(q4 E E q4))
        (transition ID '(q5 0 R q12))
        (transition ID '(q5 1 R q12))
        (transition ID '(q5 B R q5))
        (transition ID '(q5 $ $ q5))
        (transition ID '(q5 Z 0 q24))
        (transition ID '(q24 0 L q13))
        (transition ID '(q5 E 1 q25))
        (transition ID '(q25 1 L q13))
        (transition ID '(q6 0 R q7))
        (transition ID '(q6 1 R q7))
        (transition ID '(q6 B R q6))
        (transition ID '(q6 $ $ q6))
        (transition ID '(q6 Z 0 q26))
        (transition ID '(q26 0 L q8))
        (transition ID '(q6 E 1 q27))
        (transition ID '(q27 1 L q8))
        (transition ID '(q7 0 R q7))
        (transition ID '(q7 1 R q7))
        (transition ID '(q7 B L q8))
        (transition ID '(q7 $ $ q7))
        (transition ID '(q7 Z 0 q28))
        (transition ID '(q28 0 L q8))
        (transition ID '(q7 E 1 q29))
        (transition ID '(q29 1 L q8))
        (transition ID '(q8 0 Z q30))
        (transition ID '(q30 Z L q9))
        (transition ID '(q8 1 E q31))
        (transition ID '(q31 E L q9))
        (transition ID '(q8 B Z q32))
        (transition ID '(q32 Z L q10))
        (transition ID '(q8 $ $ q8))
        (transition ID '(q8 Z Z q8))
        (transition ID '(q8 E E q8))
        (transition ID '(q9 0 L q9))
        (transition ID '(q9 1 L q9))
        (transition ID '(q9 B L q10))
        (transition ID '(q9 $ $ q9))
        (transition ID '(q9 Z Z q9))
        (transition ID '(q9 E E q9))
        (transition ID '(q10 0 R q11))
        (transition ID '(q10 1 R q11))
        (transition ID '(q10 B L q10))
        (transition ID '(q10 $ B q33))
        (transition ID '(q33 B R q16))
        (transition ID '(q10 Z Z q10))
        (transition ID '(q10 E E q10))
        (transition ID '(q11 0 0 q11))
        (transition ID '(q11 1 1 q11))
        (transition ID '(q11 B L q4))
        (transition ID '(q11 $ $ q11))
        (transition ID '(q11 Z Z q11))
        (transition ID '(q11 E E q11))
        (transition ID '(q12 0 R q12))
        (transition ID '(q12 1 R q12))
        (transition ID '(q12 B L q13))
        (transition ID '(q12 $ $ q12))
        (transition ID '(q12 Z 0 q34))
        (transition ID '(q34 0 L q13))
        (transition ID '(q12 E 1 q35))
        (transition ID '(q35 1 L q13))
        (transition ID '(q13 0 E q36))
        (transition ID '(q36 E L q9))
        (transition ID '(q13 1 Z q37))
        (transition ID '(q37 Z L q14))
        (transition ID '(q13 B E q38))
        (transition ID '(q38 E L q10))
        (transition ID '(q13 $ $ q13))
        (transition ID '(q13 Z Z q13))
        (transition ID '(q13 E E q13))
        (transition ID '(q14 0 1 q39))
        (transition ID '(q39 1 L q9))
        (transition ID '(q14 1 0 q40))
        (transition ID '(q40 0 L q15))
        (transition ID '(q14 B 1 q41))
        (transition ID '(q41 1 L q10))
        (transition ID '(q14 $ $ q14))
        (transition ID '(q14 Z Z q14))
        (transition ID '(q14 E E q14))
        (transition ID '(q15 0 1 q42))
        (transition ID '(q42 1 L q9))
        (transition ID '(q15 1 0 q43))
        (transition ID '(q43 0 L q15))
        (transition ID '(q15 B 1 q44))
        (transition ID '(q44 1 L q10))
        (transition ID '(q15 $ $ q15))
        (transition ID '(q15 Z Z q15))
        (transition ID '(q15 E E q15))
        (transition ID '(q16 0 B q45))
        (transition ID '(q45 B R q17))
        (transition ID '(q16 1 R q18))
        (transition ID '(q16 B R q16))
        (transition ID '(q16 $ $ q16))
        (transition ID '(q16 Z 0 q46))
        (transition ID '(q46 0 L q19))
        (transition ID '(q16 E 1 q47))
        (transition ID '(q47 1 L q19))
        (transition ID '(q17 0 B q48))
        (transition ID '(q48 B R q17))
        (transition ID '(q17 1 R q18))
        (transition ID '(q17 B B q17))
        (transition ID '(q17 $ $ q17))
        (transition ID '(q17 Z B q49))
        (transition ID '(q49 B L q19))
        (transition ID '(q17 E 1 q50))
        (transition ID '(q50 1 L q19))
        (transition ID '(q18 0 R q18))
        (transition ID '(q18 1 R q18))
        (transition ID '(q18 B R q16))
        (transition ID '(q18 $ $ q18))
        (transition ID '(q18 Z 0 q51))
        (transition ID '(q51 0 L q19))
        (transition ID '(q18 E 1 q52))
        (transition ID '(q52 1 L q19))
        (transition ID '(q19 0 L q19))
        (transition ID '(q19 1 L q19))
        (transition ID '(q19 $ $ q19))
        (transition ID '(q19 Z Z q19))
        (transition ID '(q19 E E q19))
        (transition ID '(q19 B B q53))
        ))
        (PID ID (if (not ID) PID ID))
        )
        ((not ID) PID)
        (if trac
           (print (list (reverse (first ID))(second ID)(third ID))))))

;*****************************************************************
;* Turing Machine FULL ADDER, recursive implementation
;* ...see iterative version...
;*****************************************************************
(defun adderTM-rec (ID &optional trac)
   (if trac
      (print (list (reverse (first ID))(second ID)(third ID))))
   (let
     ((YID (or
        (transition ID '(q0 0 L q1))
        (transition ID '(q0 1 L q1))
        (transition ID '(q0 B $ q20))
        (transition ID '(q20 $ R q2))
        (transition ID '(q0 $ $ q0))
        (transition ID '(q0 Z Z q0))
        (transition ID '(q0 E E q0))
        (transition ID '(q1 0 0 q1))
        (transition ID '(q1 1 1 q1))
        (transition ID '(q1 B $ q21))
        (transition ID '(q21 $ R q2))
        (transition ID '(q1 $ $ q1))
        (transition ID '(q1 Z Z q1))
        (transition ID '(q1 E E q1))
        (transition ID '(q2 0 R q3))
        (transition ID '(q2 1 R q3))
        (transition ID '(q2 B R q2))
        (transition ID '(q2 $ $ q2))
        (transition ID '(q2 Z Z q2))
        (transition ID '(q2 E E q2))
        (transition ID '(q3 0 R q3))
        (transition ID '(q3 1 R q3))
        (transition ID '(q3 B L q4))
        (transition ID '(q3 $ $ q3))
        (transition ID '(q3 Z Z q3))
        (transition ID '(q3 E E q3))
        (transition ID '(q4 0 B q22))
        (transition ID '(q22 B R q6))
        (transition ID '(q4 1 B q23))
        (transition ID '(q23 B R q5))
        (transition ID '(q4 B B q4))
        (transition ID '(q4 $ $ q4))
        (transition ID '(q4 Z Z q4))
        (transition ID '(q4 E E q4))
        (transition ID '(q5 0 R q12))
        (transition ID '(q5 1 R q12))
        (transition ID '(q5 B R q5))
        (transition ID '(q5 $ $ q5))
        (transition ID '(q5 Z 0 q24))
        (transition ID '(q24 0 L q13))
        (transition ID '(q5 E 1 q25))
        (transition ID '(q25 1 L q13))
        (transition ID '(q6 0 R q7))
        (transition ID '(q6 1 R q7))
        (transition ID '(q6 B R q6))
        (transition ID '(q6 $ $ q6))
        (transition ID '(q6 Z 0 q26))
        (transition ID '(q26 0 L q8))
        (transition ID '(q6 E 1 q27))
        (transition ID '(q27 1 L q8))
        (transition ID '(q7 0 R q7))
        (transition ID '(q7 1 R q7))
        (transition ID '(q7 B L q8))
        (transition ID '(q7 $ $ q7))
        (transition ID '(q7 Z 0 q28))
        (transition ID '(q28 0 L q8))
        (transition ID '(q7 E 1 q29))
        (transition ID '(q29 1 L q8))
        (transition ID '(q8 0 Z q30))
        (transition ID '(q30 Z L q9))
        (transition ID '(q8 1 E q31))
        (transition ID '(q31 E L q9))
        (transition ID '(q8 B Z q32))
        (transition ID '(q32 Z L q10))
        (transition ID '(q8 $ $ q8))
        (transition ID '(q8 Z Z q8))
        (transition ID '(q8 E E q8))
        (transition ID '(q9 0 L q9))
        (transition ID '(q9 1 L q9))
        (transition ID '(q9 B L q10))
        (transition ID '(q9 $ $ q9))
        (transition ID '(q9 Z Z q9))
        (transition ID '(q9 E E q9))
        (transition ID '(q10 0 R q11))
        (transition ID '(q10 1 R q11))
        (transition ID '(q10 B L q10))
        (transition ID '(q10 $ B q33))
        (transition ID '(q33 B R q16))
        (transition ID '(q10 Z Z q10))
        (transition ID '(q10 E E q10))
        (transition ID '(q11 0 0 q11))
        (transition ID '(q11 1 1 q11))
        (transition ID '(q11 B L q4))
        (transition ID '(q11 $ $ q11))
        (transition ID '(q11 Z Z q11))
        (transition ID '(q11 E E q11))
        (transition ID '(q12 0 R q12))
        (transition ID '(q12 1 R q12))
        (transition ID '(q12 B L q13))
        (transition ID '(q12 $ $ q12))
        (transition ID '(q12 Z 0 q34))
        (transition ID '(q34 0 L q13))
        (transition ID '(q12 E 1 q35))
        (transition ID '(q35 1 L q13))
        (transition ID '(q13 0 E q36))
        (transition ID '(q36 E L q9))
        (transition ID '(q13 1 Z q37))
        (transition ID '(q37 Z L q14))
        (transition ID '(q13 B E q38))
        (transition ID '(q38 E L q10))
        (transition ID '(q13 $ $ q13))
        (transition ID '(q13 Z Z q13))
        (transition ID '(q13 E E q13))
        (transition ID '(q14 0 1 q39))
        (transition ID '(q39 1 L q9))
        (transition ID '(q14 1 0 q40))
        (transition ID '(q40 0 L q15))
        (transition ID '(q14 B 1 q41))
        (transition ID '(q41 1 L q10))
        (transition ID '(q14 $ $ q14))
        (transition ID '(q14 Z Z q14))
        (transition ID '(q14 E E q14))
        (transition ID '(q15 0 1 q42))
        (transition ID '(q42 1 L q9))
        (transition ID '(q15 1 0 q43))
        (transition ID '(q43 0 L q15))
        (transition ID '(q15 B 1 q44))
        (transition ID '(q44 1 L q10))
        (transition ID '(q15 $ $ q15))
        (transition ID '(q15 Z Z q15))
        (transition ID '(q15 E E q15))
        (transition ID '(q16 0 B q45))
        (transition ID '(q45 B R q17))
        (transition ID '(q16 1 R q18))
        (transition ID '(q16 B R q16))
        (transition ID '(q16 $ $ q16))
        (transition ID '(q16 Z 0 q46))
        (transition ID '(q46 0 L q19))
        (transition ID '(q16 E 1 q47))
        (transition ID '(q47 1 L q19))
        (transition ID '(q17 0 B q48))
        (transition ID '(q48 B R q17))
        (transition ID '(q17 1 R q18))
        (transition ID '(q17 B B q17))
        (transition ID '(q17 $ $ q17))
        (transition ID '(q17 Z B q49))
        (transition ID '(q49 B L q19))
        (transition ID '(q17 E 1 q50))
        (transition ID '(q50 1 L q19))
        (transition ID '(q18 0 R q18))
        (transition ID '(q18 1 R q18))
        (transition ID '(q18 B R q16))
        (transition ID '(q18 $ $ q18))
        (transition ID '(q18 Z 0 q51))
        (transition ID '(q51 0 L q19))
        (transition ID '(q18 E 1 q52))
        (transition ID '(q52 1 L q19))
        (transition ID '(q19 0 L q19))
        (transition ID '(q19 1 L q19))
        (transition ID '(q19 $ $ q19))
        (transition ID '(q19 Z Z q19))
        (transition ID '(q19 E E q19))
        (transition ID '(q19 B B q53))     
     )))
     (if YID
        (adderTM-rec YID trac)
        ID)))

;*****************************************************************
;* Turing Machine REVERSER, iterative implementation
;*
;* TM info:
;*    Alphabet: {x y z B $}
;*    Initial status: Q0
;*
;* Parameters:
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (reverserTM `((),(cons 'Q0 'B)(X X X Y Y Z Z)))
;* 
;*      ((B B B B B B B) (Q31 . B) (Z Z Y Y X X X))
;*    >
;*****************************************************************        
(defun reverserTM (ID &optional trac)
   (do*((ID ID
        (or
        (transition ID '(q0 x L q1))
        (transition ID '(q0 y L q1))
        (transition ID '(q0 z L q1))
        (transition ID '(q0 B R q0))
        (transition ID '(q0 $ $ q0))
        (transition ID '(q1 x x q1))
        (transition ID '(q1 y y q1))
        (transition ID '(q1 z z q1))
        (transition ID '(q1 B $ q17))
        (transition ID '(q17 $ R q2))
        (transition ID '(q1 $ $ q1))
        (transition ID '(q2 x R q2))
        (transition ID '(q2 y R q2))
        (transition ID '(q2 z R q2))
        (transition ID '(q2 B L q3))
        (transition ID '(q2 $ $ q2))
        (transition ID '(q3 x B q18))
        (transition ID '(q18 B R q7))
        (transition ID '(q3 y B q19))
        (transition ID '(q19 B R q4))
        (transition ID '(q3 z B q20))
        (transition ID '(q20 B R q6))
        (transition ID '(q3 B B q3))
        (transition ID '(q3 $ $ q3))
        (transition ID '(q4 x x q4))
        (transition ID '(q4 y y q4))
        (transition ID '(q4 z z q4))
        (transition ID '(q4 B y q21))
        (transition ID '(q21 y L q5))
        (transition ID '(q4 $ $ q4))
        (transition ID '(q5 x B q22))
        (transition ID '(q22 B R q8))
        (transition ID '(q5 y B q23))
        (transition ID '(q23 B R q13))
        (transition ID '(q5 z B q24))
        (transition ID '(q24 B R q9))
        (transition ID '(q5 B L q5))
        (transition ID '(q5 $ B q25))
        (transition ID '(q25 B R q15))
        (transition ID '(q6 x x q6))
        (transition ID '(q6 y y q6))
        (transition ID '(q6 z z q6))
        (transition ID '(q6 B z q26))
        (transition ID '(q26 z L q5))
        (transition ID '(q6 $ $ q6))
        (transition ID '(q7 x x q7))
        (transition ID '(q7 y y q7))
        (transition ID '(q7 z z q7))
        (transition ID '(q7 B x q27))
        (transition ID '(q27 x L q5))
        (transition ID '(q7 $ $ q7))
        (transition ID '(q8 x R q10))
        (transition ID '(q8 y R q10))
        (transition ID '(q8 z R q10))
        (transition ID '(q8 B R q8))
        (transition ID '(q8 $ $ q8))
        (transition ID '(q9 x R q12))
        (transition ID '(q9 y R q12))
        (transition ID '(q9 z R q12))
        (transition ID '(q9 B R q9))
        (transition ID '(q9 $ $ q9))
        (transition ID '(q10 x R q10))
        (transition ID '(q10 y R q10))
        (transition ID '(q10 z R q10))
        (transition ID '(q10 B x q28))
        (transition ID '(q28 x L q14))
        (transition ID '(q10 $ $ q10))
        (transition ID '(q11 x R q11))
        (transition ID '(q11 y R q11))
        (transition ID '(q11 z R q11))
        (transition ID '(q11 B y q29))
        (transition ID '(q29 y L q14))
        (transition ID '(q11 $ $ q11))
        (transition ID '(q12 x R q12))
        (transition ID '(q12 y R q12))
        (transition ID '(q12 z R q12))
        (transition ID '(q12 B z q30))
        (transition ID '(q30 z L q14))
        (transition ID '(q12 $ $ q12))
        (transition ID '(q13 x R q11))
        (transition ID '(q13 y R q11))
        (transition ID '(q13 z R q11))
        (transition ID '(q13 B R q13))
        (transition ID '(q13 $ $ q13))
        (transition ID '(q14 x L q14))
        (transition ID '(q14 y L q14))
        (transition ID '(q14 z L q14))
        (transition ID '(q14 B L q5))
        (transition ID '(q14 $ $ q14))
        (transition ID '(q15 x L q16))
        (transition ID '(q15 y L q16))
        (transition ID '(q15 z L q16))
        (transition ID '(q15 B R q15))
        (transition ID '(q15 $ $ q15))
        (transition ID '(q16 $ $ q16))
        (transition ID '(q16 x x q31))
        (transition ID '(q16 y y q31))
        (transition ID '(q16 z z q31))
        (transition ID '(q16 B B q31))
        ))
        (PID ID (if (not ID) PID ID))
        )
        ((not ID) PID)
        (if trac
           (print (list (reverse (first ID))(second ID)(third ID))))))
           
;*****************************************************************
;* Turing Machine REVERSER, recursive implementation
;* ...see iterative version...
;*****************************************************************           
(defun reverserTM-rec (ID &optional trac)
   (if trac
      (print (list (reverse (first ID))(second ID)(third ID))))
   (let
     ((YID (or
        (transition ID '(q0 x L q1))
        (transition ID '(q0 y L q1))
        (transition ID '(q0 z L q1))
        (transition ID '(q0 B R q0))
        (transition ID '(q0 $ $ q0))
        (transition ID '(q1 x x q1))
        (transition ID '(q1 y y q1))
        (transition ID '(q1 z z q1))
        (transition ID '(q1 B $ q17))
        (transition ID '(q17 $ R q2))
        (transition ID '(q1 $ $ q1))
        (transition ID '(q2 x R q2))
        (transition ID '(q2 y R q2))
        (transition ID '(q2 z R q2))
        (transition ID '(q2 B L q3))
        (transition ID '(q2 $ $ q2))
        (transition ID '(q3 x B q18))
        (transition ID '(q18 B R q7))
        (transition ID '(q3 y B q19))
        (transition ID '(q19 B R q4))
        (transition ID '(q3 z B q20))
        (transition ID '(q20 B R q6))
        (transition ID '(q3 B B q3))
        (transition ID '(q3 $ $ q3))
        (transition ID '(q4 x x q4))
        (transition ID '(q4 y y q4))
        (transition ID '(q4 z z q4))
        (transition ID '(q4 B y q21))
        (transition ID '(q21 y L q5))
        (transition ID '(q4 $ $ q4))
        (transition ID '(q5 x B q22))
        (transition ID '(q22 B R q8))
        (transition ID '(q5 y B q23))
        (transition ID '(q23 B R q13))
        (transition ID '(q5 z B q24))
        (transition ID '(q24 B R q9))
        (transition ID '(q5 B L q5))
        (transition ID '(q5 $ B q25))
        (transition ID '(q25 B R q15))
        (transition ID '(q6 x x q6))
        (transition ID '(q6 y y q6))
        (transition ID '(q6 z z q6))
        (transition ID '(q6 B z q26))
        (transition ID '(q26 z L q5))
        (transition ID '(q6 $ $ q6))
        (transition ID '(q7 x x q7))
        (transition ID '(q7 y y q7))
        (transition ID '(q7 z z q7))
        (transition ID '(q7 B x q27))
        (transition ID '(q27 x L q5))
        (transition ID '(q7 $ $ q7))
        (transition ID '(q8 x R q10))
        (transition ID '(q8 y R q10))
        (transition ID '(q8 z R q10))
        (transition ID '(q8 B R q8))
        (transition ID '(q8 $ $ q8))
        (transition ID '(q9 x R q12))
        (transition ID '(q9 y R q12))
        (transition ID '(q9 z R q12))
        (transition ID '(q9 B R q9))
        (transition ID '(q9 $ $ q9))
        (transition ID '(q10 x R q10))
        (transition ID '(q10 y R q10))
        (transition ID '(q10 z R q10))
        (transition ID '(q10 B x q28))
        (transition ID '(q28 x L q14))
        (transition ID '(q10 $ $ q10))
        (transition ID '(q11 x R q11))
        (transition ID '(q11 y R q11))
        (transition ID '(q11 z R q11))
        (transition ID '(q11 B y q29))
        (transition ID '(q29 y L q14))
        (transition ID '(q11 $ $ q11))
        (transition ID '(q12 x R q12))
        (transition ID '(q12 y R q12))
        (transition ID '(q12 z R q12))
        (transition ID '(q12 B z q30))
        (transition ID '(q30 z L q14))
        (transition ID '(q12 $ $ q12))
        (transition ID '(q13 x R q11))
        (transition ID '(q13 y R q11))
        (transition ID '(q13 z R q11))
        (transition ID '(q13 B R q13))
        (transition ID '(q13 $ $ q13))
        (transition ID '(q14 x L q14))
        (transition ID '(q14 y L q14))
        (transition ID '(q14 z L q14))
        (transition ID '(q14 B L q5))
        (transition ID '(q14 $ $ q14))
        (transition ID '(q15 x L q16))
        (transition ID '(q15 y L q16))
        (transition ID '(q15 z L q16))
        (transition ID '(q15 B R q15))
        (transition ID '(q15 $ $ q15))
        (transition ID '(q16 $ $ q16))
        (transition ID '(q16 x x q31))
        (transition ID '(q16 y y q31))
        (transition ID '(q16 z z q31))
        (transition ID '(q16 B B q31))
     )))
     (if YID
        (reverserTM-rec YID trac)
        ID)))

;*****************************************************************
;* Turing Machine UNARY ADDER, iterative implementation
;*
;* TM info:
;*    Alphabet: {1 B}
;*    Initial status: Q1
;*
;* Parameters:
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (adderTM `((),(cons 'Q1 '1)(1 B 1 1)))
;* 
;*      ((B 1 B) (Q3 . B) (1))
;*    >
;*****************************************************************
(defun unary-adderTM (ID &optional trac)
   (do*((ID ID
        (or
        (transition ID '(q1 1 B q1))
        (transition ID '(q1 B R q2))
        (transition ID '(q2 1 R q2))
        (transition ID '(q2 B R q3))
        (transition ID '(q3 1 B q3))
        ))
        (PID ID (if (not ID) PID ID))
        )
        ((not ID) PID)
        (if trac
           (print (list (reverse (first ID))(second ID)(third ID))))))


;*****************************************************************
;* Turing Machine UNARY SUBTRACTION, iterative implementation
;*
;* TM info:
;*    Alphabet: {1 B}
;*    Initial status: Q1
;*
;* Parameters:
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (unary-subtractionTM `((),(cons 'Q1 '1)(1 1 B 1 1)))
;* 
;*      ((1 B B) (Q5 . B) (B B B))
;*    >
;*****************************************************************
(defun unary-subtractionTM (ID &optional trac)
   (do*((ID ID
        (or
        (transition ID '(q1 1 B q1))
        (transition ID '(q1 B R q2))
        (transition ID '(q2 1 R q2))
        (transition ID '(q2 B R q3))
        (transition ID '(q3 1 R q3))
        (transition ID '(q3 B L q4))
        (transition ID '(q4 1 B q4))
        (transition ID '(q4 B L q5))
        (transition ID '(q5 1 L q6))
        (transition ID '(q6 1 L q6))
        (transition ID '(q6 B L q7))
        (transition ID '(q7 1 L q8))
        (transition ID '(q7 B R q9))
        (transition ID '(q8 1 L q8))
        (transition ID '(q8 B R q1))
        (transition ID '(q9 B R q9))
        (transition ID '(q9 1 L q9))
        ))
        (PID ID (if (not ID) PID ID))
        )
        ((not ID) PID)
        (if trac
           (print (list (reverse (first ID))(second ID)(third ID))))))
           
;*****************************************************************
;* Turing Machine PROPER UNARY SUBTRACTION, iterative
;* implementation
;*
;* TM info:
;*    Alphabet: {1 B}
;*    Initial status: Q1
;*
;* Parameters:
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (proper-unary-subtractionTM `((),(cons 'Q1 '1)(1 B 1 1 1)))
;* 
;*      ((B B B B) (Q10 . B) (B B))
;*    >
;*****************************************************************
(defun proper-unary-subtractionTM (ID &optional trac)
   (do*((ID ID
        (or
        (transition ID '(q1 1 B q1))
        (transition ID '(q1 B R q2))
        (transition ID '(q2 1 R q2))
        (transition ID '(q2 B R q3))
        (transition ID '(q3 1 R q3))
        (transition ID '(q3 B L q4))
        (transition ID '(q4 1 B q4))
        (transition ID '(q4 B L q5))
        (transition ID '(q5 1 L q6))
        (transition ID '(q6 1 L q6))
        (transition ID '(q6 B L q7))
        (transition ID '(q7 1 L q8))
        (transition ID '(q7 B R q9))
        (transition ID '(q8 1 L q8))
        (transition ID '(q8 B R q1))
        (transition ID '(q9 B R q10))
        (transition ID '(q10 1 B q9))
        ))
        (PID ID (if (not ID) PID ID))
        )
        ((not ID) PID)
        (if trac
           (print (list (reverse (first ID))(second ID)(third ID))))))


;*****************************************************************
;* Turing Machine INTERPRETER, iterative
;*
;* Parameters:
;*    'TM'  : list of quadruples
;*    'ID'  : as described above...
;*    'trac': boolean value, set as non-nil (optional) to trace
;*            the computation
;* 
;* Output:
;*    Final ID, if such ID exists.
;* 
;* Usage example at console:
;*    >
;*    > (interpreter *adder-TM* `((),(cons 'Q0 '1)(B 1 1 1)))
;* 
;*      ((B) (Q19 . B) (1 0 0 0))
;*    >
;*****************************************************************
(defun interpreter (TM ID &optional trac)
   (do((YID ID))((not YID)ID)
      (dolist (quad TM)
         (setf YID (transition ID quad))
         (when YID
            (setf ID YID)
            (if trac (print (list (reverse (first ID))(second ID)(third ID))))
            (return)))))


;*****************************************************************
;* Turing Machine UNARY ADDER, program (invoke with interpreter)
;*****************************************************************
(defvar *unary-add-TM* '(
   (q1 1 B q1)
   (q1 B R q2)
   (q2 1 R q2)
   (q2 B R q3)
   (q3 1 B q3)))

;*****************************************************************
;* Turing Machine PROPRER SUBTRACTION, program (invoke with interpreter)
;*****************************************************************
(defvar *proper-unary-sub-TM* '(
   (q1 1 B q1)
   (q1 B R q2)
   (q2 1 R q2)
   (q2 B R q3)
   (q3 1 R q3)
   (q3 B L q4)
   (q4 1 B q4)
   (q4 B L q5)
   (q5 1 L q6)
   (q6 1 L q6)
   (q6 B L q7)
   (q7 1 L q8)
   (q7 B R q9)
   (q8 1 L q8)
   (q8 B R q1)
   (q9 B R q10)
   (q10 1 B q9)))

;*****************************************************************
;* Turing Machine UNARY SUBTRACTION, program (invoke with interpreter)
;*****************************************************************
(defvar *unary-sub-TM* '(
   (q1 1 B q1)
   (q1 B R q2)
   (q2 1 R q2)
   (q2 B R q3)
   (q3 1 R q3)
   (q3 B L q4)
   (q4 1 B q4)
   (q4 B L q5)
   (q5 1 L q6)
   (q6 1 L q6)
   (q6 B L q7)
   (q7 1 L q8)
   (q7 B R q9)
   (q8 1 L q8)
   (q8 B R q1)
   (q9 B R q9)
   (q9 1 L q9)))
   
;*****************************************************************
;* Turing Machine REVERSER, program (invoke with interpreter)
;*****************************************************************
(defvar *reverser-TM* '(
   (q0 x L q1)
   (q0 y L q1)
   (q0 z L q1)
   (q0 B R q0)
   (q0 $ $ q0)
   (q1 x x q1)
   (q1 y y q1)
   (q1 z z q1)
   (q1 B $ q17)
   (q17 $ R q2)
   (q1 $ $ q1)
   (q2 x R q2)
   (q2 y R q2)
   (q2 z R q2)
   (q2 B L q3)
   (q2 $ $ q2)
   (q3 x B q18)
   (q18 B R q7)
   (q3 y B q19)
   (q19 B R q4)
   (q3 z B q20)
   (q20 B R q6)
   (q3 B B q3)
   (q3 $ $ q3)
   (q4 x x q4)
   (q4 y y q4)
   (q4 z z q4)
   (q4 B y q21)
   (q21 y L q5)
   (q4 $ $ q4)
   (q5 x B q22)
   (q22 B R q8)
   (q5 y B q23)
   (q23 B R q13)
   (q5 z B q24)
   (q24 B R q9)
   (q5 B L q5)
   (q5 $ B q25)
   (q25 B R q15)
   (q6 x x q6)
   (q6 y y q6)
   (q6 z z q6)
   (q6 B z q26)
   (q26 z L q5)
   (q6 $ $ q6)
   (q7 x x q7)
   (q7 y y q7)
   (q7 z z q7)
   (q7 B x q27)
   (q27 x L q5)
   (q7 $ $ q7)
   (q8 x R q10)
   (q8 y R q10)
   (q8 z R q10)
   (q8 B R q8)
   (q8 $ $ q8)
   (q9 x R q12)
   (q9 y R q12)
   (q9 z R q12)
   (q9 B R q9)
   (q9 $ $ q9)
   (q10 x R q10)
   (q10 y R q10)
   (q10 z R q10)
   (q10 B x q28)
   (q28 x L q14)
   (q10 $ $ q10)
   (q11 x R q11)
   (q11 y R q11)
   (q11 z R q11)
   (q11 B y q29)
   (q29 y L q14)
   (q11 $ $ q11)
   (q12 x R q12)
   (q12 y R q12)
   (q12 z R q12)
   (q12 B z q30)
   (q30 z L q14)
   (q12 $ $ q12)
   (q13 x R q11)
   (q13 y R q11)
   (q13 z R q11)
   (q13 B R q13)
   (q13 $ $ q13)
   (q14 x L q14)
   (q14 y L q14)
   (q14 z L q14)
   (q14 B L q5)
   (q14 $ $ q14)
   (q15 x L q16)
   (q15 y L q16)
   (q15 z L q16)
   (q15 B R q15)
   (q15 $ $ q15)
   (q16 $ $ q16)
   (q16 x x q31)
   (q16 y y q31)
   (q16 z z q31)
   (q16 B B q31)))

;*****************************************************************
;* Turing Machine FULL ADDER, program (invoke with interpreter)
;*****************************************************************
(defvar *adder-TM* '(
   (q0 0 L q1)
   (q0 1 L q1)
   (q0 B $ q20)
   (q20 $ R q2)
   (q0 $ $ q0)
   (q0 Z Z q0)
   (q0 E E q0)
   (q1 0 0 q1)
   (q1 1 1 q1)
   (q1 B $ q21)
   (q21 $ R q2)
   (q1 $ $ q1)
   (q1 Z Z q1)
   (q1 E E q1)
   (q2 0 R q3)
   (q2 1 R q3)
   (q2 B R q2)
   (q2 $ $ q2)
   (q2 Z Z q2)
   (q2 E E q2)
   (q3 0 R q3)
   (q3 1 R q3)
   (q3 B L q4)
   (q3 $ $ q3)
   (q3 Z Z q3)
   (q3 E E q3)
   (q4 0 B q22)
   (q22 B R q6)
   (q4 1 B q23)
   (q23 B R q5)
   (q4 B B q4)
   (q4 $ $ q4)
   (q4 Z Z q4)
   (q4 E E q4)
   (q5 0 R q12)
   (q5 1 R q12)
   (q5 B R q5)
   (q5 $ $ q5)
   (q5 Z 0 q24)
   (q24 0 L q13)
   (q5 E 1 q25)
   (q25 1 L q13)
   (q6 0 R q7)
   (q6 1 R q7)
   (q6 B R q6)
   (q6 $ $ q6)
   (q6 Z 0 q26)
   (q26 0 L q8)
   (q6 E 1 q27)
   (q27 1 L q8)
   (q7 0 R q7)
   (q7 1 R q7)
   (q7 B L q8)
   (q7 $ $ q7)
   (q7 Z 0 q28)
   (q28 0 L q8)
   (q7 E 1 q29)
   (q29 1 L q8)
   (q8 0 Z q30)
   (q30 Z L q9)
   (q8 1 E q31)
   (q31 E L q9)
   (q8 B Z q32)
   (q32 Z L q10)
   (q8 $ $ q8)
   (q8 Z Z q8)
   (q8 E E q8)
   (q9 0 L q9)
   (q9 1 L q9)
   (q9 B L q10)
   (q9 $ $ q9)
   (q9 Z Z q9)
   (q9 E E q9)
   (q10 0 R q11)
   (q10 1 R q11)
   (q10 B L q10)
   (q10 $ B q33)
   (q33 B R q16)
   (q10 Z Z q10)
   (q10 E E q10)
   (q11 0 0 q11)
   (q11 1 1 q11)
   (q11 B L q4)
   (q11 $ $ q11)
   (q11 Z Z q11)
   (q11 E E q11)
   (q12 0 R q12)
   (q12 1 R q12)
   (q12 B L q13)
   (q12 $ $ q12)
   (q12 Z 0 q34)
   (q34 0 L q13)
   (q12 E 1 q35)
   (q35 1 L q13)
   (q13 0 E q36)
   (q36 E L q9)
   (q13 1 Z q37)
   (q37 Z L q14)
   (q13 B E q38)
   (q38 E L q10)
   (q13 $ $ q13)
   (q13 Z Z q13)
   (q13 E E q13)
   (q14 0 1 q39)
   (q39 1 L q9)
   (q14 1 0 q40)
   (q40 0 L q15)
   (q14 B 1 q41)
   (q41 1 L q10)
   (q14 $ $ q14)
   (q14 Z Z q14)
   (q14 E E q14)
   (q15 0 1 q42)
   (q42 1 L q9)
   (q15 1 0 q43)
   (q43 0 L q15)
   (q15 B 1 q44)
   (q44 1 L q10)
   (q15 $ $ q15)
   (q15 Z Z q15)
   (q15 E E q15)
   (q16 0 B q45)
   (q45 B R q17)
   (q16 1 R q18)
   (q16 B R q16)
   (q16 $ $ q16)
   (q16 Z 0 q46)
   (q46 0 L q19)
   (q16 E 1 q47)
   (q47 1 L q19)
   (q17 0 B q48)
   (q48 B R q17)
   (q17 1 R q18)
   (q17 B B q17)
   (q17 $ $ q17)
   (q17 Z B q49)
   (q49 B L q19)
   (q17 E 1 q50)
   (q50 1 L q19)
   (q18 0 R q18)
   (q18 1 R q18)
   (q18 B R q16)
   (q18 $ $ q18)
   (q18 Z 0 q51)
   (q51 0 L q19)
   (q18 E 1 q52)
   (q52 1 L q19)
   (q19 0 L q19)
   (q19 1 L q19)
   (q19 $ $ q19)
   (q19 Z Z q19)
   (q19 E E q19)
   (q19 B B q53)))   
   
;*****************************************************************
;* Turing Machine COMPILER, iterative
;*
;* Parameters:
;*    'TM'  : list of quadruples
;*    'name': name of the compiled lisp function
;* 
;* Output:
;*    Lisp function (lambda/lambda-block).
;* 
;* Usage example at console (DIRECT INVOCATION):
;*    >
;*    > (funcall (compiler *adder-TM* 'name) `((),(cons 'Q0 '1)(B 1 1 1)))
;* 
;*      ((B) (Q19 . B) (1 0 0 0))
;*    >
;* Usage example at console (LATE INVOCATION):
;*    >
;*    > (compiler *adder-TM* 'adderFUN)
;*
;*      ADDERFUN
;*    > (adderFUN `((),(cons 'Q0 '1)(B 1 1 1)))
;*
;*      ((B) (Q19 . B) (1 0 0 0))
;*    >
;*****************************************************************
(defun compiler (TM &optional name)
   (eval (let ((chain-or '(or)))
      (dolist (quad TM)
         (setf chain-or
            (append chain-or
               (list(list 'transition 'ID `(quote ,quad))))))
      (append (if name `(defun ,name) '(lambda))
              '((ID &optional trac))
              `((do*((ID ID ,chain-or)
                 (PID ID (if (not ID) PID ID)))
                 ((not ID) PID)
                 (if trac
                    (print(list
                        (reverse
                           (first ID))
                           (second ID)
                           (third ID))))))))))

;Prints a Christmas tree made of stars '*'
(defun xmas (n)
   (do ((i 1 (+ i 2)) (j 1 (+ 1 j)))((> j n))
      (do ((r 1 (+ 1 r)))((> r (- n j))) (format t " "))
      (dotimes (k i)(format t "*"))
      (format t "~%")
   )
)

;Computes the Ackermann function that is computable but
;not primitive recursive
(defun ackermann (m n)
   (format t "(~d,~d)~%" m n)
   (cond
      ((= m 0) (+ n 1))
      ((= n 0) (ackermann (- m 1) 1))
      (t (ackermann (- m 1) (ackermann m  (- n 1))))
   )
)
