; 7 単純なリストの先へ
; 奇妙なリスト
; Lispのリストはコンスセルから作られる
; コンスセルは２つのデータを結びつける構造で、リストの最後のコンスセルの右のスロットはnilになっている
; 例えば、数値1,2,3からなるリストを作りたいときはこう
(cons 1 (cons 2 (cons 3 nil)))

; ドットリスト
; コンスセルの連なりから外れてみる
(cons 1 (cons 2 3))
; (1 2 . 3)
; 正式なリストの最後にあるべきnilが見つからなかったことを示すため、lispは最後の要素の前にドットをおいて表示した
; このドットによってLispは「君がくれた構造をリストとして表示しようとしたんだけど、リストの一番最後にnilじゃなくて３を見つけたんだ」ということを示している
; nil以外の値でリストが終端されているものはドットリストと呼ばれる
; コンスセルの最終端がnilでない値のときもあるので、慣れておく必要がある
; 実際通常のリストを次の通りドット表記で書くことだってできる
'(1 . (2 . (3 . nil)))
; (1 2 3)
; この考え方でいけば、ドットリストのドットは、Lispがリストを表示する際に最後のコンスセルだけを明示するためにつけたものとも言える

; 対
; Lispプログラムでよく使われる実用的なドットリストは、対を簡潔に表現するためのもの
; 例えば2と3の対を表現したいとすると
(cons 2 3)
; (2 . 3)
; こうしてつくると、この対から値を取り出すときに標準のcarとcdrが使えるから便利
; また２つの要素をつなぐために一つだけコンスセルをアロケートすればいいので、比較的効率がいい
; この形の対はLispでよく使われる
; 2次元の店のx座標とy座標、複雑なデータの中のキーと値の対（連想リスト）など

; 循環リスト
; リストの最後をnilではなく最初のコンスセルを指すようにしたらどうなるか
; リスト中のコンスセルはそれぞれメモリ上に独立したオブジェクトとして存在していると考えられる
; コンスセルのcarとcdrのスロットはメモリ上の他の好きなオブジェクトをさせるから、自分が属するリストの前の方を指すことだってありえる
; こういうリストを循環リストという

; 連想リスト
; 別名alist,alistはキーと値の対のリスト
; 慣習的にリスト中に同じキーが複数現れている場合、最初の物だけを有効と見なすことが多い
; これは３人のコーヒーの注文を表現するalist
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)
    )
)

; 特定の人の注文を得るにはassocを使えばいい
(assoc 'lisa *drink-order*)

; この関数はリストの最初から順にマッチするキーを探し、見つかればキーと値の対を返す
; ここで同じキーのlisaに対してpush関数を使って注文を変更できる
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

; push関数はすでにあるリストのまえに新しい陽を追加するだけだ
; 通常は連想リストで最初に見つかったキーの値が優先されるからLisaの最初の注文はあとの注文に取って代わられることになる
; (phpでは上書きされるのでそこだけ違う)
; さらに一度alistに格納された値は決して消えないので、後からデータの変更履歴をたどることも簡単である
; ただし、alistは小さなリスト（十数項目以内）を扱っているのではない限り、データを取り出すのにあまり効率のいい方法ではないということである
; この効率の問題で、プログラムが成熟するにつれ他のデータ型に置き換えられることが多い(9章)

; 7.2 複雑なデータを扱うには
; コンスセルによるデータの表現と可視化はLispの設計の中心にあるため、
; コンスセルからなる構造を使ったりデバックしたりするのはとても簡単である
; 実のところ、たとえ性能が要求される場面でもコンスセルで構造を組むことはいい選択になり得る
; Lispのコンパイラはコンスセルの変更をアセンブリの１命令にまで最適化することができるからだ

; 木構造のデータの可視化
; Lispのデータ（とコード）は式の構文によって表現される
; この構文では、データはネストしたリストで表現され、通常リストの先頭にはその構造が何を表すかを示すシンボルが置かれる
; 例として家の構成部品をLispで表現してみよう
(defparameter *house* '((walls (morter (cement)
                                       (water)
                                       (sand))
                                (bricks))
                                (windows (glass)
                                         (frame)
                                         (curtains))
                                (roof (shingles)
                                      (chimney)
                        ))
)

; グラフを可視化する
; 数学ではグラフとはエッジにより接続されたノードの集合だ
; ノードやエッジには付加的なデータが結び付けられることもある
; こういったグラフをコンスセルで格納することは簡単だが、それをわかりやすく見せるのは難しい
; ５章で使っていた魔法館のマップをこの形式（有向グラフ）で表現するのに２つのalistを用いた
; １つ目はノードの情報を保持し、２つ目はエッジの情報を保持していた
; ここではこの２つのalistを*wizard-nodes*と*wizard-edges*に改名して使うことにする

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                                (garden (you are in a beautiful garden.
                                        there is a well in fron of you.))
                                (attic (you are in the attic. there is a giant welding torch in the corner.))
                              )
)

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                                (garden (living-room east door))
                                (attic (living-room downstairs ladder))
                               )
)

; このデータの生の形式を見るだけでは、ゲーム世界の構造をきちんと把握するのは難しいだろう
; こういったグラフ構造を持っていたり、単純な木構造に様々な属性が付加されているような複雑なデータというものは珍しくない
; これを可視化できるツールがあるのでこれからはそれを使ってみる

; 7.3 グラフを作る
; Graphvizはデータからグラフを生成してくれるツールだ
; Graphviz使えるようになったので、Lispから簡単にグラフをかけるライブラリを作ってみよう
; それを使えばゲーム世界のグラフを得られる

; DOTの情報を生成する
; グラフ描画ライブラリを生成するにはまずグラフのもつ情報をすべて記載したgraphvizのDOTファイルを生成しなければならない
; そのためにはプレーヤーが到達可能なノードの識別子とそれらをつなぐエッジをDOTのフォーマットに変換し、各ノードとエッジに付加するラベルをつけてやる必要がある
; 以下では魔法使いの世界のマップを表すノードを使ってこれから作っていくライブラリをテストしていく

; ノードの識別子を変換する
; ノードをDOTフォーマットに変換するにはまずノードの識別子をDOTの識別子として有効な形に変換してやることだ
; これをするのがdot-name関数である
(defun dot-name (exp)
    (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp))
)
; DOTフォーマットのノードの名前にはアルファベット、数字そしてアンダースコアしか使えない
; 出力するノードの名前がその制限を満たすようにDOTが受け付けない文字をすべてアンダースコアに変更してしまおう
(dot-name 'living-rom)
; => "LIVING_ROOM"
; この関数はLispの基本的な型のオブジェクトなら何でも受け取れる
; prin1-to-stringによってそれを文字列に変換しているからだ
; 続いてその文字列を処理して必要な箇所をアンダースコアに置換している
; substitute-ifは与えられたテスト関数の結果によって値を置き換える関数である
(substitute-if #\e #'digit-char-p "I'm a l33t hack3r!")
; => I'm a leet hacker!
; substitute-ifはリストも処理できる
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8))
; => (0 2 0 4 0 6 0 8)
; substitute-ifは様々なデータ型のオブジェクトを引数として受取りそれぞれに適切な処理を行うことができる、ジェネリック関数の一つである(9章)
; Lispにはすでにとある文字が「アルファベットか数字である」という条件を判別する述語alphanumericpが備わっている
; これの補集合（complement）で、「アルファベットでも数字でもない」文字を得られる
; つまりこう書ける(alphanumbericpを高階関数complementにわたすことで作られる)
(complement #'alphanumericp)

; グラフのノードにラベルを付ける
; 次にラベルを生成する関数を書く
; ラベルにはノードの名前と、ノードのalistに格納されているデータを表示することにしよう
; ただラベル中にテキストを詰め込みすぎないように気をつけなければならない
; いかがラベルを生成するコードである
(defun dot-label (exp)
    (if exp
        (let ((s (write-to-string exp :pretty nil)))
            (if (> (length s) *max-label-length*)
                (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
                s
            )
        )
        ""
    )
)
; *max-label-length*はラベルに表示する最大文字数を決めるグローバル変数である
; もしラベルの文字数がこの制限を超えたら、ラベルの文字列を切り詰めて省略されたことを示すために"..."をつける
; write-to-string関数はprin1-to-stringと同様、式を文字列に書き出すものだ
; :pretty引数は渡したい引数を指定して渡せる、キーワード引数の一つ
; write-to-stringでは、この指定で文字列に余分な文字を付け加えないようにしている
; この指定がないとLispは文字列を整形して表示してしまう
; :pretty引数にnilを渡すのはそのような装飾を避けよという指示になる

; ノードのDOT情報を生成する
(defun nodes->dot (nodes)
    (mapc (lambda (node)
            (fresh-line)
            (princ (dot-name (car node)))
            (princ "[label=\"")
            (princ (dot-label node))
            (princ "\"];"))
        nodes
    )
)
; この関数はmapcを使って、リスト中のノードを一つずつ取り上げ、変換した情報をprincで直接スクリーンに出力する
; mapcはmapcarよりも少し高速な関数
; 違いはmapcarのように結果のリストを返さないというところ
; nodes->dot関数は、実際の変換には上で作ったdot-nameとdot-labelを使っている
; 後でこれらの情報をファイルに書き出すことになるが、その際はコンソールに出力される情報を横取りする関数を書く

; エッジをDOTフォーマットに変換する
(defun edges->dot (edges)
    (mapc (lambda (node)
            (mapc (lambda (edge)
                    (fresh-line)
                    (princ (dot-name (car node)))
                    (princ "->")
                    (princ (dot-name (car edge)))
                    (princ "[label=\"")
                    (princ (dot-label (cdr edge)))
                    (princ "\"];"))
                (cdr node)
              )
            )
    edges
    )
)

; DOTデータを完成させる
(defun graph->dot (nodes edges)
    (princ "digraph{")
    (nodes->dot nodes)
    (edges->dot edges)
    (princ "}")
)

; DOTファイルを画像にする
; DOTファイルをビットマップの画像にするには、生成したDOT形式のデータをファイルに落とし、シェルからdotコマンドを起動してやればいい
(defun dot->png (fname thunk)
    (with-open-file (*standard-output*
                     fname
                     :direction :output
                     :if-exists :supersede
                     )
        (funcall thunk)
    )
    (ext:shell (concatenate 'string "dot -Tpng -O " fname))
)
; ファイルへの出力
; with-open-fileによってdot->pngは情報をファイルに書き出す
; この関数の動作を理解するために、testfile.txtというファイルを新しく作ってその中に"Hello File!"と書き出す例を示す
(with-open-file (my-stream 
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
    (princ "Hello File!" my-stream)
)
; with-open-fileの最初に渡されているシンボルは、with-open-fileによって作られる、common Lispでストリームと呼ばれるデータ型を持つオブジェクトになる

; ストリームをつくる 
; princ等の出力関数は省略可能な引数としてストリームを受け取る
; すると関数はコンソールではなく、そのストリームオブジェクトへとデータを出力する
; letが与えられt変数名を持つ変数を作るのと同じように、with-open-fileが与えられた変数名を持つストリーム変数を作っているということが重要だ
; ストリームとはファイルに結び付けられて、princ等の関数に渡せばそのファイルへとデータを書き出す事ができるってことだけ知っていればOK

; キーワード引数を理解する
; with-open-fileをもう一度みてみよう
; それぞれのキーワード引数は２つの部分からなる 引数の名前と渡す値である
; キーワード引数の名前はいつでも、コロンで始まるシンボルだ
; :direction引数には値:outputが渡され、ファイルを読み込みではなく、書き出し用にオープンすることを示す
; :if-exists引数には値:supersedeが渡され、同名のファイルが既に存在していた場合は以前の内容を捨てるように指示している
; with-open-fileがたくさんのキーワード引数を取るのは、ファイルをオープンするというのは複雑な操作で、いろいろな場合を想定したオプションが必要だからだ
; キーワードシンボルの再定義は許されない