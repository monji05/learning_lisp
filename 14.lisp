; 数学的な関数には以下の重要な性質ある、Lispの関数もできるだけに従っている
; 関数は渡される引数が同じであれば、常に同じ結果を返す（参照透過性という）
; 関数は外部の変数を決して参照しない(array_map()を思い出す)
; 関数呼び出しが変数の値を変えることはない（これもarray_map()）
; 関数の目的は引数から値を計算することである
; 関数はそれ以外に外の世界に影響を与えない
; 関数は外の世界からの情報を受け取らない
; プログラムコードが外の世界に見えるなにかコンピューターを「ピー」と鳴らすなり、スクリーンにダイアログを出すようなコードは副作用を持つという
; 副作用を持つ汚いコードは技術用語では「命令的」と呼ばれる
; 手順書がそうであるように命令的なコードはほとんどの行で副作用を期待している
; ここから、関数型プログラミングの中心となる原理が導かれる、すなわちプログラムを２つの部分に分割すべしということ
; 1. はじめにして大なる部分は関数型プログラミングで書くべしこの部分に副作用を含むべからず
; 2. 終わりにして小さき部分はすべての副作用を含み、ユーザー及び外の世界との交渉を担うべし、この部分は不浄にして、可能な限り小さく保つべし

; 14.2 関数型スタイルで書かれたプログラムの分析
; 関数型の部分
(defun add-widget (database widget)
    (cons widget database)
)

; 副作用を持つ汚いコード
(defparameter *database* nil)

(defun main-loop()
    (loop (princ "Please enter the name of a new widget:")
        (setf *database* (add-widget *database* (read)))
        (format t "The database contains the fllowing: ~a~%" *database*)
    )
)
; 例えば１００万この品物を格納するとする
; データベースサーバーを複製して新しい品物を追加したものを返すと解釈する
; ただ、add-widgetは「ずる」をする
; 新しい品物を追加する先頭以降のリストは以前のリストでいいので、以前のリストには触れずにその先頭に新たな品物をコンスするだけで、「新しい」リストを返すということができる
; add-widgetがアロケートするメモリは、新しい品物と以前のリストをつなぐためのコンスセル一つだけだ
; 新たなデータ構造を作るときにこんなふうに「ずる」をすることが、効率的な関数型プログラミングを可能にする重要なテクニックで、重要な性質である
; こうやってadd-widgetは毎回データを追加した新たなデータベースを返す
; 不浄な部分にåるmain-loopがその結果を受取り、グローバル変数*database*を、新しいデータベースを指すように変更する
; つまり、データベースは次の２ステップで間接的に変更される
; 1. add-widget関数はいわばこのプログラムの頭脳であり、新たなデータベースを作り出す
; 2. main-loop関数が汚れ仕事を引き受ける部分でグローバル変数*database*を変更することで操作を完了する

; 14.3 高階プログラミング
; 関数型プログラマ初心者は一つの動作を完成させるためにどうやってコードの断片を組み立てたらいいのかわからない
; これはコードの合成と言われる
; 関数型プログラミングで最も強力なコード合瀬の道具は関数を引数として受取る関数を使う高階プログラミングである
; 関数型プログラマ初心者にとってコード合成がなぜ難しいのか例をつかって考えてみる
; 次に示すリストが与えられたときにその各要素に２を加えたリストがほしいとする
(defparameter *my-list* '(4 7 2 3))
; そのためにはリストの要素をひとつずつ見ていくコードと、要素に２を加えるというコードが必要だ
; そしてその２つを組み合わせる

; 命令型コードでのコード合成
(loop for n below (length *my-list*)
    do (setf (nth n *my-list*) (+ (nth n *my-list*) 2))
)
; このコードには主に２つのデメリットがある
; 1. 元のリストを破壊しているので、他の場所で*my-list*を使っていると問題になる
;    Lisperはこのコードのように*my-list*が知らないあいだに変化してしまうようなことを、「プログラムに隠された状態がある」という
;    隠された状態から生まれるバグは、命令型プログラミングを主体とするプログラミング言語では非常に多いバグだ
; 2. リストの現在の位置を指定するために一時的な変数nを用意しなければならない
;    こういった変数はコードを膨らませ、バグが忍び込む余地を増やす、またうっかりnに不正な値を代入してしまったり、使い方を間違えてリストの意図せざる要素をアクセスしてしまうといった誤りの危険もある

; 関数型スタイルを使う
; まずは高階プログラミングを使わない方法で書いてみる
; add-twoは渡されたリストの先頭の要素に2を足し、残りの要素に対しては自分自身を再帰呼び出して処理を行う
; このコードは命令型コードのデメリットを回避している
; だが、命令型コードにあった、要素に２を足すコードとリストを走査するというコードとがはっきり分かれていない
; それがadd-twoとうこの仕事のためだけの関数をわざわざ定義しなければならない原因だ
(defun add-two (list)
    (when list
        (cons (+ 2 (car list)) (add-two (cdr list)))
    )
)

; 高階プログラミングによる救援
; このコードは関数型でかつリストを走査するという仕事と加算するという仕事がきれいに分かれている
; リストを走査するのはmapcar関数で、これはリストの各要素に引数で渡された関数を適用するという、高階関数だ
; 加算するという仕事はlambdaで作られる関数でコードされていて、ここではその数がリストから来たのかどうかは関知しない
(mapcar (lambda (x)
            (+ x 2)
        )
        '(4 7 2 3)
)

; 14.4 関数型プログラミングはなぜクレージーか
; 関数型プログラミングはものすごく非効率になり得る
; その問題を解決するためにメモ化、末尾呼び出し最適化、遅延評価、高階プログラミングがそうだ

; 14.5 関数型プログラミングはなぜ素晴らしいか 
; 関数型プログラミングはバグを減らす
; 関数型プログラミングでは関数の振る舞いに影響を与えるのはただ一つ、関数に渡した引数だ
; 関数の振る舞いが引数だけに依存するように書いておくと、バグを再現するのも簡単である同じ引数で呼び出せば、必ず同じふるまいになるはずだからだ(参照透過性)