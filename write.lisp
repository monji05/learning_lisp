(let ((a 5)
      (b 6))
    (+ a b))


(flet ((f (n)
         (+ n 10)))
  (f 5)
  )

(flet ((f (n)
          (+ n 10))
        (g (n)
          (- n 3)))
  (g (f 5))
)

(labels ((a (n)
           (+ n 5))
         (b (n)
           (+ (a n) 6))
         )
  (b 10)
  )

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0)
  )
(my-length '(list with four symbols))
(my-length ())

(if (oddp 2)
    'odd-number
    'not-odd
    )


(defun checkodd (a)
  (if (oddp a)
    'odd-number
    a
  )
)
(checkodd 10)

(and 
 (oddp 5)
 (oddp 7)
 (oddp 9)
 )

(defparameter *fruit* 'apple)
(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange)
)






(defparameter *nodes* '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))
                        )
)

(assoc 'garden *nodes*)


(defun describe-location (location nodes)
  (cadr (assoc location nodes))

)

(describe-location 'living-room *nodes*)


(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder)
                                     (garden (living-room east door))
                                     (attic (living-room downstairs ladder))
                                     ))
)

(defun describe-path (edge)
   '(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))


;;; p64
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))
)
(describe-paths 'living-room *edges*)
