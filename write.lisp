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
                                     (attic upstairs ladder))
                                     (garden (living-room east door))
                                     (attic (living-room downstairs ladder))
                                     )
)

(defun describe-path (edge)
   '(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))


;;; 1. find related edge
;;; 2. convert edge to the drawing
;;; 3. put these drawings
;;; location current user's position
;;; edges game map's edges
;;; cdr means array_values in php
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))
)
(describe-paths 'living-room *edges*)
(cdr (assoc 'living-room *edges*))

(mapcar #'sqrt '(1 2 3 4 5))

;;; #'は functionオペレータの略記
(mapcar #'car '((foo bar) (baz qux)))

(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux)))
)

(append '(mary had) '(a) '(little lamb))

;;; applyはリストの各要素を引数として関数を呼び出したように動作する
;;; applyはネストしたリスト'((mary had) (a) (little lamb))とappendをガムテープでくっつけているようなもの
(apply #'append '((mary had) (a) (little lamb)))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '(
                                  (whiskey living-room)
                                  (bucket living-room)
                                  (chain garden)
                                  (frog garden)
                                 )
)

;;; 関数がnilか真の値を返す場合、その関数の最後にpをつける習わしがある
;;; 真偽値を確かめる関数は述語(predicate)と呼ばれるのでそれが由来
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))

    (remove-if-not #'at-loc-p objs)
  )
)

(objects-at 'living-room *objects* *object-locations*)


;;; シンボルは`で、データモードは'ややこしい
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)
             ))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))
    )
)

(describe-objects 'living-room *objects* *object-locations*)


(defparameter *location* 'living-room)


(defun look () (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)
  )
)

(defun walk (derection)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr
                    )
        ))
  )
  (if next
      (progn (self *location* *edges*))
              (look))
  '(you cannot go that way.
  )
)

(defun pickup (object)
  (cond ((member object
          (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))
        )
)
