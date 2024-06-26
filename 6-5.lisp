; 6.5.1 lambdaがすること
; 簡単に言えばlabmdaを使えば、名前を与えずに関数を作ることができる
; 例えば、渡された数を半分にする関数を書いてみる
(defun half (n)
    (/ n 2)
)
; half関数そのものを呼び出すときはfunction オペレータが使える
#'half

; lambdaコマンドはこの２ステップを一度にやってしまう
; 名前を介さずに、関数を定義してその関数そのものを得る
(lambda (n)
    (/ n 2)
)
; こうして無名の関数を値として得られたら、それをmacpcarやapply等、他のCommon Lispコマンドに直接渡すことができる
; 例えばリストの中の数値をすべて半分にするのはこんなふうにきれいに書ける
(mapcar (lambda (n) (/ n 2)) '(2 4 6))

; lambdaの引数は評価されずにlambdaに渡される、つまりlambdaは本物の関数ではない
; これはマクロと呼ばれるものである
; 2章でLispの関数の引数は関数自体が評価される前にすべて評価されるって学んだが、それに対しマクロはちょっと違う(16章を参照)
; さらにややこしいのが、lambdaが返すのは通常のLisp関数ってことだ
; 「ラムダ関数」について話すときはlambdaによって作られた無名関数のことで、lambdaマクロ自身のことでない
; lambdaを使えるとプログラム自体を一段高い概念で書けるということ


; 6.5.2 lambdaがそんなに大事なわけ
; 関数を普通のデータのように渡すことになれると、プログラムの設計を全く新しい方向から見ることができる
; 関数を値として渡されるプログラミングスタイルは高階プログラミングと呼ばれる（14章）
; 純に数学的な意味では、lambdaが唯一のLispコマンドっていえる
; Lispは普通の言語と違い、ラムダ算法という数学的な概念から直接導かれたプログラミング言語だと言った
; ラムダ算法とはlambdaを唯一のコマンドとして特別なコード変換によって完全なプログラミング言語をつくれる
; つまりLispの関数はすべてlambdaが根源である