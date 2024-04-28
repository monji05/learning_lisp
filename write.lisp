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


;;; assocで alistから第一引数のキーに該当する値を返す
(defun describe-location (location nodes)
  (cadr (assoc location nodes))

)

(describe-location 'living-room *nodes*)


;;; phpでいうとこんな感じ？
;;; array (
;;;  "living-room" => array (
;;;  [0] => "garden",
;;; [1] => "upstairs",
;;; [2] => "ladder"
;;;  )
;;; )
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                                     (garden (living-room east door))
                                     (attic (living-room downstairs ladder))
                                     )
)

;;; 準クォートの仕組み
;;; これまでコードモードからデータモードの切替に使っていたシングルクォートの代わりにバッククォートを使う
;;; シングルクォートもバッククォートも、Lispではコードの一部をデータモードに切り替えられる
;;; 違いはバッククォートの場合その中でコンマを使うことで一部分だけコードモードに戻せる（要は変数展開のようなもの）
;;; これをアンクォートという
(defun describe-path (edge)
   `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))


;;; 1. find related edge
;;; 2. convert edge to the drawing
;;; 3. put these drawings
;;; location current user's position
;;; edges game map's edges
;;; cdr means array_values in php
;;;
;;; mapcarはそれぞれのリストの最初の要素を取り出しリストにして返している
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
;;; labelsコマンドでローカル関数at-loc-pを定義
;;; at-loc-p関数はオブジェクトの名前を表すシンボルを取り、それが場所locにあるかどうかをtかnilで返す
;;; assocはalist(association list または縮めてalist)からキーと値のペアを返す
;;; その仕組はまずオブジェクトをobj-locs alistから探し次にeqを使って現在の場所とオブジェクトの場所が一致するかどうかみる
;;; 最後の行にあるremove-if-not関数は渡されたリストの各要素に第一引数の関数(ここではat-loc-p)を適用しそれが真の値を返さなかったものを除いたリストを得るもの phpでいうarray_filter()みたいなものか
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))

    (remove-if-not #'at-loc-p objs)
  )
)

;;; 同じ結果が返る、もしかして一緒？なわけないか、エラーになったことあったし
(objects-at `living-room *objects* *object-locations*)
(objects-at 'living-room *objects* *object-locations*)


;;; シンボルは`で、データモードは'ややこしい
;;; describe-objを定義して、「与えられたオブジェクトが床にある」という文を準クォートを使って作り出す
;;; そして関数の本体では現在の場所にあるオブジェクトをobjects-at関数使って見つけ、そのオブジェクトのリストに対してdescribe-objをマップして
;;; 最後にappendですべての描写をつなげて一つのリストにしている
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

; 見つけた通り道をまず変数nextに格納し, if式によってnextが値を持っているかどうか（nextがnilでないかどうか）を検査する
; nextが値を持っていれば渡された方向は有効なものだったということだから,プレイヤーの現在地を更新する
; lookの呼び出しは更新された場所の描写を作り出し、それが結果の値として返される
; プレイヤーが無効な通り道を指定した場合はwalkは場所の描写をせずにそちらには進めないというメッセージを返す
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)
        ))
    (if next
        (progn (setf *location* (car next))
                (look))
        '(you cannot go that way.)
    )
  )
)

; pickupはmember関数を使ってオブジェクトが確かに現在地の床にあるかを確かめる(member関数はリストの中に要素があるかどうかを検査するのに使える)
; 現在地にあるオブジェクトのリストを得るのにobject−atを使っている
; オブジェクトが現在地にあれば、pushコマンドを使ってオブジェクトと新しい場所からなるリストをobject-locationsに付け足す
; 新しい場所とはプレーヤーの体を表すbody
; pushコマンドはsetfコマンドを使って作られた簡易関数である
(defun pickup (object)
  (cond ((member object
          (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))
        )
)

; プレイヤーが現在もっている物を見られる関数
; objects-atを使ってある場所に存在するオブジェクトのリストを取ってきている
; オブジェクトをプレーヤーが手に取ったときそのオブジェクトの場所を'bodyにしたことを思い出そう
; そこが、この関数で探すべき場所である
(defun inventory ()
  (cons 'items (objects-at 'body *objects* *object-locations*))
)

; 5.8 学んだこと
; ゲームの世界ではプレーヤーが行くことができる場所をノードとし、場所感を行き来する経路をエッジとする数学的なグラフで表現できる
; このグラフは変数*nodes*に連想リスト（alist）の形で持っていくことができる。これにより、ノード（場所）の名前からその場所の属性を引ける。
; このゲームでは属性として、各ノード（場所）の描写を格納しておいた
; assoc関数によりキー（ここでは場所の名前）を使ってalistからデータを引き出す事ができる
; 準クォートを使えば、大きなデータの中に、その一部分を計算するためのコードを埋めることができる
; Lispの関数には他の関数を非キスとして受け取るものがあるこれらは高階関数と呼ばれる
; mapcarはcommon Lispで最もよく使われる高階関数である
; alist中の値を置き換えたければ、新しい要素をリストにpushするだけでいい、assocは最も新しい値だけを返すからだ
