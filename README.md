# Labda

Labda はプログラミング言語かつ定理証明支援系である。

## 名前の由来

"Labda" は "lambda" の古い語形が由来である。

## 基本設計

### Labda はプログラミング言語かつ定理証明支援系である。

一つ目に、プログラミング言語において、そのプログラムの正確性を保証する究極の機構とは、定理証明支援系である。二つ目に、定理証明支援系において、その証明を記述する究極の機構とは、プログラミング言語である。ならば、この二種類のシステムを融合させることで、両方の利益を最短で活用することができるだろう。

### Labda はラムダ計算を基礎とする。

プログラムのモデルには様々な種類がある。それらの中でも重要なものを、それぞれが出現した順番に並べると、以下のようになる。

1. コンビネータ論理
1. ラムダ計算
1. チューリングマシン
1. レジスタマシン

どのモデルが Labda の基礎として適切なのだろうか？　先人を見やると、 Coq や Agda や Lean などのプログラミング言語としての側面も持つ定理証明支援系は、その全てがラムダ計算を基礎にしている。このため、 Labda もそれに倣う。

### Labda は構成主義を採用する。

Labda は定理証明支援系とプログラミング言語が一体となったものである。そのため、ある整合的な証明は何らかの実行可能なプログラムに対応していなければならないし、ある実行可能なプログラムは何らかの整合的な証明に対応していなければならない。これは、構成主義に他ならない。

### Labda の意味論は Labda のプログラムをラムダ計算の項に変換する写像で定める。

Labda はラムダ計算を基礎としているが、その上に様々な機能を組み込む予定でもある。たとえば、帰納型や余帰納型などのような機能のことである。しかしながら、これらの機能は、時には通常のラムダ計算から逸脱した動作を実現する。そのため、 Labda の意味論をラムダ計算への変換により定めることで、そのプログラムの意味を明確にし、ラムダ計算から離れすぎないようにする。

### Labda はラカーセアーを基礎とする。

ラムダ計算はプログラムのモデルとして充分な能力を備えている。しかし、現実的なプログラミング言語の基礎として使うためには、幾つか欠けている機能がある。

1. 外部とのやり取り
1. 実行順序のコントロール
1. メモリの確保と解放のタイミングのコントロール

これらを組み込んだラムダ計算を、「其の副作用が明示的なラムダ計算」と呼ぶことにする。「其の副作用が明示的なラムダ計算」では長すぎるため、それを英訳した "lambda calculus whose side effects are explicit" を縮めた "lacawseae" を音写した「ラカーセアー」と呼ぶことにする。

### Labda のプログラムへラカーセアーのプログラムを埋め込むことが可能であるとする。

Labda はプログラミング言語である。 Labda をプログラミング言語として実用したいのであればチューリング完全でなければならない。そのことを Labda へラカーセアーを埋め込むことができるようにすることで明確にする。

### Labda のプログラムへ非安全なプログラムを埋め込むことが可能であるとする。

Rust は様々な安全性を厳密にチェックするが、それだけではシステムプログラミングなどが行えないため、そのチェッカを無効にする機能として "unsafe" が存在する。 Labda にも "unsafe" を組み込むことにしよう。

### Labda は非安全なプログラムをラカーセアーのプログラムの埋め込みにより実現するとする。

ラカーセアーは外部のやり取りが可能であるため、ラカーセアーを使ってシステムプログラミングを行うことも可能であり、そのラカーセアーを Labda へ埋め込むことができるというのならば、 Labda における非安全なプログラムを Labda へ埋め込んだラカーセアーのプログラムで実装するということは、理にかなっている。

Labda はラカーセアーへコンパイルされる。すなわち、 Labda とラカーセアーは Rust とアセンブラ言語の関係に似ている。ならば、 Rust のインラインアセンブラのように、 Labda の内部でラカーセアーを記述することで非安全なプログラムを書けるようにすることは、理にかなっている。

### ラカーセアーは実行順序のコントロールを可能とする。

ラムダ計算は、項書き換え系として見なすと、実行順序の曖昧さが出てくる。それが、次の二つである。

1. $` \lambda x \ldotp t `$ としたとき、そこで簡約を止めるか、あるいは $` t `$ の中まで簡約するかどうか。
1. $` f ( x ) `$ としたとき、 $` f `$ から簡約をするか、 $` x `$ から簡約をするかどうか。

ラムダ抽象を突き抜けて簡約するかどうかについては、ほとんどのプログラミング言語において、それを行なわないため、ラカーセアーでも行なわないことにしよう。

関数の適用において、その関数（左の値）と、その引数（右の値）のどちらかを先に簡約するかどうかについては、かなり厄介な問題である。

左と右のどちらを先に簡約するか？　プログラミング言語においては、どちらかをデフォルトにすることが一般的である。

Haskell は、色々と単純にするならば左の値から簡約するプログラミング言語である。つまり、遅延評価である。これは Haskell の大きな特徴である。しかし、実行時間や使用メモリ量などが全く直感的ではなく、未評価の式が膨らみ過ぎてメモリが満杯になるというスペースリークを引き起こしやすい。その問題の多さたるや、遅延評価ではなく正格評価を疑似的にデフォルトにする `Strict` 拡張というのが存在するぐらいである。だが、遅延評価により可能になるアルゴリズムも多い。

OCaml は、右の値から簡約するプログラミング言語である。つまり、正格評価である。正格評価というと、 C 言語のような引数の値を計算してから関数を計算するというのを、私はイメージする。その通りではあるのだが、たとえば `f x y` を考えたとき、これは `(f x) y` ということになるので、その評価の順序は `y, x, f` である。なので、少し注意が必要である。

遅延評価と正格評価のどっちにも良い所がある。もし、ラカーセアーにおいて、左と右のどちらかをデフォルトにしてしまえば、どちらかのメリットを得ることが出来なくなる。ならば、その場に応じて遅延評価と正格評価のどちらも使えるようにしよう。

### ラカーセアーは関数適用への注釈により評価順序を定める。

ラカーセアーでは遅延評価と正格評価のどちらも使えるようにする。では、それをどうやって実現すればよいのだろうか。

最初に思い付いたのは、関数の適用に手を加えることである。 Haskell の [($)](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Base.html#v:-36-) と [($!)](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Base.html#v:-36--33-) のように。

それらの記号を借りることにしよう。関数の適用は二種類に分裂する。一つ目は `f $ x` であり、これは `f` から簡約をおこなうことである。二つ目は `f $! x` であり、これは `x` から簡約をおこなうことである。

実際のユースケースで考えてみる。 `f x y` と書いたとき、この評価順序をコントロールすることを考えてみよう。 `f, x, y` の順で評価させたい時は、 `(f $ x) $ y` と書けばよい。 `y, x, f` の順で評価させたい時は、 `(f $! x) $! y` と書けばよい。普通の手続き型プログラミング言語のように `x, y, f` の順で評価させたい時は……

`(f $! x) $ y` と書けばいけると思ってたけど、これは `x, f, y` になる。同じように `(f $ x) $! y` は `y, f, x` という順番になる。

`x, y, f` の順で評価させたいならば、 `(\ x_ -> (f $ x_) $! y) $! x` とすればよいが、やや複雑である。

別の方法も考えてみる。ラムダ抽象に手を加えてみよう。ラムダ抽象は `\ ~x -> t` と `\ !x -> t` の二種類に分裂し、前者を適用する時は左の値から計算し、後者を適用する時は右の値から計算する。

`f x y` の評価順序を定めるためには、 `f` をある程度簡約する必要がある。 `(\ ~x_ -> \ ~y_ -> f x_ y_) x y` と書いた時は、最初に`f` の簡約があることは決まっているが、その後にある `x` と `y` の評価の順番は `f` の内容による。 `(\ !x_ -> \ ~y_ -> f x_ y_) x y` の時は、 `x, f, y` の順番になる。 `(\ ~x_ -> \ !y_ -> f x_ y_) x y` の時は、 `y, f, x` の順番になる。 `(\ !x_ -> \ !y_ -> f x_ y_) x y` の時は、 `x, y, f` の順番になる。

`f x y` の評価順序を定めるためには、 `f` のラムダ抽象が露出するまで簡約する必要がある……つまり、 `f` を WHNF まで簡約することになり、ほぼ遅延評価と同じということになってしまう。また、ラムダ抽象の中身を簡約しないはずなのに、それが遅延評価である場合に、その中身を先に簡約することになってしまう。

他の方法は、今のところ思い付いていない。ならば、上記の方法の内、どれがよいのか。一番目の方法には簡約の順序を操作するのがスムーズにいかないという欠点はあるが、それは二番目の方法も同じだし、二番目の方法は実質的に遅延評価となってしまうという欠点が大きすぎるため、一番目の方法を採用することにする。

### ラカーセアーはメモリの確保と解放のタイミングをコントロールできるものとする。

ラムダ計算を実際に実行することを考えると、たとえば、 `( \ x_ -> t ) x` を簡約する時に、 `t` の中で `x_` が 2 箇所以上で現れているとすると、 `x` を複製しなければならない。 `t` の中に `x_` が現れていないとすると、 `x` を破棄しなければならない。

では、どのようなタイミングで値の複製と破棄……つまりメモリの確保と解放をすればよいのだろうか？　最後の最後まで複製をしないことも、出来る限り早いタイミングで複製することも出来る。

普通のプログラミング言語なら、それをコンパイラに任せるのが普通である。しかし、ラカーセアーは、 Labda のコンパイル先となる言語であり、普段は隠れているものをコントロールできるようにするというのがコンセプトなのである。

### ラカーセアーは線形ラムダ計算と特別な式を組み合せてメモリ操作のタイミングをコントロールする。

値の複製と破棄、つまりメモリの確保と解放をコントロールすることを考える。まず、ラムダ抽象が導入する全ての変数が、その後の式の中で一回しか現れないのであれば、値の複製と破棄は発生しない。そのため、ラムダ抽象で導入した変数は必ず一回だけ使用するものとする。これは線形ラムダ計算である。

だが、線形ラムダ計算のままでは表現力が弱すぎる。そこで、値の複製を行なう式と値の破棄を行なう式を導入することにする。 `duplicate y, z <- x then t` は、 `x` の値を `y` と `z` へ複製して `t` の計算で使うことを意味するとする。 `destruct x then t` は、 `t` を計算する前に `x` を破棄することを意味するとする。もちろん、 `duplicate` 式が導入する変数も必ず一回だけ使用することになる。ある変数は、関数適用の関数か引数となるか、 `destruct` 式で破棄となるかとなる。

たとえば、最小の簡約が停止しない式は `( \ x -> duplicate y, z <- x then y z ) ( \ x -> duplicate y, z <- x then y z )` となる。

### ラカーセアーは副作用を発生させる関数を持つものとする。

ファイル操作やインターネット接続などを実装するためには、副作用が必須である。副作用のコントロールをするための手法として IO モナドなどがある。しかし、 Labda のコンパイル先がラカーセアーであるため、ラカーセアーの仕様は極々単純にしなければならない。そのためにラカーセアーは型なしラムダ計算を基本としている。 IO モナドには型が必要であるため、 IO モナドを使うことは出来ない。そのため、ラカーセアーは、それを簡約した際に副作用が発生する関数を持つものとする。

### Labda は、プログラミング言語のユースケースを大体カバーできるものとする。

Labda は、プログラミング言語と定理証明支援系を融合させることで、両者の利益を活用するのが目的である。そのため、 Labda は、プログラミング言語に定理証明支援系を組み込んで利益が出るユースケースを全てカバーできるものでなければならない。そして、その定理証明支援系とは型システムに他ならないので、型システムを導入して利益が出るユースケースを全てカバーできるものでなければならない。そして、そのようなユースケースは、システムプログラミングやアプリケーションプログラミングやウェブプログラミングなどの広範に渡る。

### Labda は実行可能ファイルを生成することが出来るものとする。

Labda は、アプリケーションプログラミングが可能であるものとしている。そのため、アプリケーションプログラミングを行なうために必要なものとして、実行可能ファイルを生成することが出来なければならない。

### Labda を開発する際には、成果物を出力する部分から逆順に辿りながら作っていく。

Labda のコンパイラは基本的にソースコードから実行可能ファイルを生成するプログラムであると過程してみると、次のような段階を経ることになる。

1. ソースコード
1. 構文木
1. 抽象構文木
1. 中間表現 A
1. 中間表現 B
1. 中間表現 C
1. 実行可能ファイル

そして、基本的にソースコードが多様多種な表現を持つのに対して実行可能ファイルは単純な表現しか持たない。たとえば、ソースコードでは改行や括弧などを用いた記述をするのに対して、実行可能ファイルでは機械語を用いた直線的な記述を行なうことになる。たとえば、 C 言語のソースコードでは `if` 文や `if-else` 文や `for` 文や `while` 文や `do-while` 文などの多様多種な構文があるが、実行可能ファイルでは `goto` 命令のみを用いてプログラムの流れを制御することになる。

それは実装の際にも同様であり、ソースコードから構文木への部分を実装する時には、ソースコードと構文木を定義して、その間を繋ぐパーサーを定義しなければならないが、中間表現 C から実行可能ファイルへの部分を実装する時には、中間表現 C を定義して、実行可能ファイルとの間を繋ぐプログラムを定義するだけで済むのである。また、ソースコードを定義することを考えると、プログラムの表層たる構文を定めるための「自動車置き場の議論」じみた選定が必要な上に、後から定義を変更すると抽象構文木の定義までに波及する可能性も高いのだが、中間表現 C を定義することを考えると、内部表現であるため在る程度の変更が効く上に、いくつかの変換を挟んだ後の単純な表現であることを前提にして定義しても良いため、容易に定義することが可能である。

実行可能ファイル以外のものを成果物に設定しても、同じようなことが成り立つ。これが、成果物を出力する部分から逆順に辿りながら作っていくことに決めた、一つ目の理由である。

ソースコードから順番に定義していく場合を考えてみよう。構文を決めるというのは厄介な作業だし、それを元にしてパーサーを定義するのも厄介な作業である。構文木とパーサーを定義するのは作業量が単純に多いし、新しくプログラミング言語を開発する場合は、構文が揺らいでいる場合が多く、一通りの構文を定義しても後で変更となる可能性が高くて「完成した」という達成感がいまいちなさそうである。

それに対して、最初から成果物を出力する部分を定義した場合、成果物が目に見えるためモチベーションが上がりやすそうである。これが、成果物を出力する部分から逆順に辿りながら作っていくことに決めた、二つ目の理由である。

### Labda は実行順序をコントロールできる機能を持つものとする。

正格評価と遅延評価のどちらにもメリットとデメリットがあるので、その両者を扱えるようにしたい。また、ラカーセアーは実行順序をコントロールすることが可能であるので、そこへコンパイルする Labda でも活かしたい。また、 Haskell が好きなので、遅延評価をデメリットが少ない形で取り込みたい。

### Labda は関数型への注釈で実行順序をコントロールするものとする。

どうやって、 Labda で実行順序をコントロールすればよいのだろうか　ラカーセアーでは、「関数適用への注釈」と「ラムダ抽象への注釈」の両者で検討した上で、「関数適用への注釈」を選択した。しかし、 Labda においては型システムが存在するため、第三の選択肢がある。

それは「関数型への注釈」である。具体的には、 $` f ( x ) `$ と書いた時に、次のようにすることである。

1. $` f : ( x : A )_L \rightarrow B ( x ) `$ であれば、 $` f `$ を先に簡約する。
1. $` f : ( x : A )_R \rightarrow B ( x ) `$ であれば、 $` x `$ を先に簡約する。

この挙動はコンパイル時に `f x` を `f $ x` と `f $! x` のどちらにするかで表現可能である。このような関数型を作りたい時は $` \lambda_L ( x : A ) \ldotp t `$ または $` \lambda_R ( x : A ) \ldotp t `$ とする。この字面はラムダ抽象への注釈にも似ているように見えるが、 $` f `$ と $` x `$ のどちらかを先に簡約するかどうかを決める時に $` f `$ をラムダ抽象が露出するまでに簡約する必要はなく、ただ型を見ればよいのである。

また、関数型への注釈という手法は、数量型理論 (quantitative type theory, QTT) の多重度にも通じており、何らかを参考にできるかもしれない。

### Labda は値の複製と破棄をコントロールする機能を持つものとする。

初歩的な範囲での Haskell は全ての値を参照で扱う富豪的プログラミングである。しかし、生の値を扱う機能もあり、性能を追求することも出来る。なので、 Labda も同様にしたい。そのためには、値の複製と破棄をコントロールする機能が必要となる。

### Labda は数量型理論で値の複製と破棄をコントロールできるものとする。

値の複製と破棄をコントロールするといえば、線形論理である。それを型理論と融合させたのが数量型理論 (quantitative type theory, QTT) である。これは Idris 2 や Linear Haskell などでも使われている。そのため、 Labda も数量型理論で値の複製と破棄をコントロールするものとする。

数量型理論は、型なしラムダ計算へ翻訳するとき、値の計算と一緒に型の受け渡しも翻訳してしまうという問題も解決してくれる。たとえば、 $` \mathrm{id} = \lambda ( A : \mathrm{Type} ) \ldotp \lambda ( x : A ) \ldotp x : ( A : \mathrm{Type} ) \rightarrow A \rightarrow A `$ を型なしラムダ計算へ翻訳すると $` \lambda A \ldotp \lambda x \ldotp x `$ となるが、この引数の $` A `$ の部分が無駄である。

これを数量型理論は多重度という枠組みで解決してくれる。多重度というのは、その引数がどれくらい使われるかの注釈である。前述の関数の場合、 $` A `$ の多重度を 0 に設定することが出来る。そして、多重度が 0 である引数は、消去することが出来るのである。 Idris 2 は、このような利点を享受している。

### Labda は外部とのやり取りをコントロールできる機能を持つものとする。

Labda は様々な場面で使用できるプログラミング言語である。そのため、 Labda は外部とのやり取りをコントロールできる機能を持たなければならない。

### Labda は IO モナドで外部とのやり取りをコントロールするものとする。

Haskell では、 IO モナドを使って純粋性を保ちながら、外部とのやり取りを可能としているのは有名な話である。 Idris や Lean や Agda なども同種の仕組みを持っている。

外部とのやり取りをコントロールする……つまり副作用をコントロールする仕組みはモナドだけではない。コモナドおよびアローでも可能である。しかし、 $` x `$ は `pure` に対応し $` ( \lambda x \ldotp t ) s `$ は `(>>=)` に対応するなど、モナドはラムダ計算と同じ構造を持つため、モナドを採用するものとする。

### Labda は非安全なプログラムを記述できるものとする。

Rust は様々な性能向上の仕組みを持っているが、それでも目的とする性能へ達することが出来ない場合や、他の言語のプログラムと直接データをやり取りしたい時に、 unsafe 機能を用いることが出来る。 Labda も低レベルなプログラミングが可能であることを目指しているため、この機能も取り込む。

### Labda は Unsafe モナドを用いて非安全なプログラムを記述できるものとする。

上記で記述したように、 Labda はラカーセアーを埋め込むことで非安全なプログラムを実現する。この埋め込みを Unsafe モナドとする。

では、 Unsafe モナドには、どのような操作があれば、ラカーセアーを埋め込めることになるのだろうか？

ラカーセアーを型付きラムダ計算として見なすと、それは関数型と $` T `$ しか型を持たず、 $` ( T \rightarrow T ) `$ が $` T `$ と等しいような理論となる。すなわち、自明な理論に対応している。

自明な理論であるということは、ありとあらゆる命題が等しいということである。すなわち、任意の型 $` A `$ と任意の型 $` B `$ に対して、 $` A \rightarrow B `$ という関数が存在する。さらに、これに多重度も加えるには継続を使えばよく、そうすると $` ( \_ :_m A ) \rightarrow ( \_ :_1 ( \_ :_n B ) \rightarrow \mathrm{Unrestricted} ( C ) ) \rightarrow \mathrm{Unrestricted} ( C ) `$ となる。（この型に `Unrestricted` が付いているのは `C` に `B` を代入して `B` の値をそのまま返そうとするのを防ぐためである。まあ、この場合は無意味かもしれないけど。）後は、この型に Unsafe モナドを付けるだけである。つまり、 $` ( \_ :_m A ) \rightarrow ( \_ :_1 ( \_ :_n B ) \rightarrow \mathrm{Unsafe} ( \mathrm{Unrestricted} ( C ) ) ) \rightarrow \mathrm{Unsafe} ( \mathrm{Unrestricted} ( C ) ) `$ となる。

### Labda は Classic モナドを用いて古典論理を記述できるものとする。

Labda は、プログラミング言語の側面を持つ定理証明支援系の例に漏れず、古典論理を特徴づける公理を証明することは出来ない。では、どのようにして古典論理を扱えばよいのだろうか？

1. 公理を簡約できない値として追加する。
1. 常に公理を仮定として含める。

一番目の方法は、妥当な証明は妥当なプログラムと対応するという設計原理と相容れない。二番目の方法は、かなり良さそうであるが、仮定を与えて議論を実体化できないのが微妙である。もっと良い方法はないのだろうか？

継続を使うことで、排中律およびパースの公理を実装できることは有名な話である。

```coq
Definition law_of_excluded_middle ( R : Type ) ( A : Type ) : ( Sum A ( A -> R ) -> R ) -> R := fun x : Sum A ( A -> R ) -> R => x ( right_Sum A ( A -> R ) ( fun y : A => x ( left_Sum A ( A -> R ) y ) ) ).

Definition peirce_s_law ( R : Type ) ( A : Type ) ( B : Type ) : ( ( A -> ( B -> R ) -> R ) -> ( A -> R ) -> R ) -> ( A -> R ) -> R := fun x : ( A -> ( B -> R ) -> R ) -> ( A -> R ) -> R => fun y : A -> R => x ( fun z : A => fun w : B -> R => y z ) c.
```

つまり、命題論理の範囲の古典論理だけなら、直観主義論理の中で実体を持った形で再現することが出来るのだ。しかし、 ZFC 集合論をやるためには、排中律だけではなく選択公理も必要である。というのも、 HoTT Book では、選択公理を仮定することで、 ZFC のモデルを作るという議論をしているのだ。

Martin Escardo によると、 HoTT の選択公理は排中律 (the law of excluded middle, LEM) と二重否定シフト (the double negation shift, DNS) へ分解できるそうだ。

> The axiom of choice in HoTT/UF is equivalent to the conjunction of the principle of excluded middle and the double negation shift. — [Martin Escardo, Mathstodon](https://mathstodon.xyz/@MartinEscardo/109546986011309833)

排中律は、前述のように継続を使って実装できる。ならば、二重否定シフトも継続を使って実装できないだろうか？　それが出来れば、 ZFC 集合論のモデルを作ることも可能になり、現在の数学の大部分を扱う土台が成り立つといえよう。

"[Delimited control operators prove Double-negation Shift](https://arxiv.org/abs/1012.0929)" によれば、二重否定シフトは限定継続を使って実装できるらしい。これを参考にして考察してみよう。

`Continue[R]` を限定継続の文脈を持つ型とする。限定継続のプリミティブとして、次の関数がある。

```txt
reset_ : Continue[A] A -> A
reset : Continue[A] A -> Continue[B] A
shift : ((A -> B) -> Continue[B] B) -> Continue[B] A
```

二重否定シフトは次のような型を持つ関数である。

```txt
double_negation_shift : ((x : A) -> ((B x -> C) -> C)) -> (((x : A) -> B x) -> C) -> C
```

これを限定継続のプリミティブを使って実装すると、次のようになる。

```txt
double_negation_shift = \ (x : (x : A) -> ((B x -> C) -> C)) -> \ (y : ((x : A) -> B x) -> C) -> reset_ (y (\ (z : A) -> shift (\ (w : B z -> C) -> x z w)))
```

この実装を詳細に見て行こう。 `Continue[C]` のあるなしで型が合わないように見えるが、 `Continue[C]` は型ではないので問題ない。

```txt
goal_0 : ((x : A) -> ((B x -> C) -> C)) -> (((x : A) -> B x) -> C) -> C
goal_0 = \ (x : (x : A) -> ((B x -> C) -> C)) -> \ (y : ((x : A) -> B x) -> C) -> reset_ (y (\ (z : A) -> shift (\ (w : B z -> C) -> x z w)))

goal_1 : (((x : A) -> B x) -> C) -> C
goal_1 = \ (y : ((x : A) -> B x) -> C) -> reset_ (y (\ (z : A) -> shift (\ (w : B z -> C) -> x z w)))

goal_2 : C
goal_2 = reset_ (y (\ (z : A) -> shift (\ (w : B z -> C) -> x z w)))

goal_3 : Continue[C] C
goal_3 = y (\ (z : A) -> shift (\ (w : B z -> C) -> x z w))

goal_4 : Continue[C] ((x : A) -> B x)
goal_4 = \ (z : A) -> shift (\ (w : B z -> C) -> x z w)

goal_5 : Continue[C] (B x)
goal_5 = shift (\ (w : B z -> C) -> x z w)

goal_6 : (B z -> C) -> Continue[C] C
goal_6 = \ (w : B z -> C) -> x z w

goal_7 : Continue[C] C
goal_7 = x z w
```

`Continue[A] B` を型で表現することを考えると、 `(B -> A) -> A` となる。この翻訳を使って限定継続のプリミティブを表した場合、次のようになる。

```txt
reset_ : ((A -> A) -> A) -> A
reset : ((A -> A) -> A) -> (A -> B) -> B
shift : ((A -> B) -> (B -> B) -> B) -> (A -> B) -> B
```

この定義に合わせて、実装を組み立て直してみよう。

```txt
goal_7 : (C -> C) -> C
goal_7 = \ (p : C -> C) -> p (x z w)

goal_6 : (B z -> C) -> (C -> C) -> C
goal_6 = \ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)

goal_5 : (B x -> C) -> C
goal_5 = shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w))

goal_4 : (((x : A) -> B x) -> C) -> C
goal_4 = \ (q : ((x : A) -> B x) -> C) -> q (\ (z : A) -> shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)))

goal_3 : (C -> C) -> C
goal_3 = \ (r : C -> C) -> (\ (q : ((x : A) -> B x) -> C) -> q (\ (z : A) -> shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)))) (\ (v : (x : A) -> B x) -> r (y v))

goal_2 : C
goal_2 = reset_ (\ (r : C -> C) -> (\ (q : ((x : A) -> B x) -> C) -> q (\ (z : A) -> shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)))) (\ (v : (x : A) -> B x) -> r (y v)))

goal_1 : (((x : A) -> B x) -> C) -> C
goal_1 = \ (y : ((x : A) -> B x) -> C) -> reset_ (\ (r : C -> C) -> (\ (q : ((x : A) -> B x) -> C) -> q (\ (z : A) -> shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)))) (\ (v : (x : A) -> B x) -> r (y v)))

goal_0 : ((x : A) -> ((B x -> C) -> C)) -> (((x : A) -> B x) -> C) -> C
goal_0 = \ (x : (x : A) -> ((B x -> C) -> C)) -> \ (y : ((x : A) -> B x) -> C) -> reset_ (\ (r : C -> C) -> (\ (q : ((x : A) -> B x) -> C) -> q (\ (z : A) -> shift (\ (w : B z -> C) -> \ (p : C -> C) -> p (x z w)))) (\ (v : (x : A) -> B x) -> r (y v)))
```

残念ながら、 `goal_4` でどうしようもないギャップが発生してしまっている。これは何故だろうか？

```math
\frac{ \Gamma \vdash_\diamond A ( x ) \qquad \textrm{\( x \)-fresh} }{ \Gamma \vdash_\diamond \forall x A ( x ) } \; \forall_l
```

このルールが問題である。継続の文脈をモナドとして具体化してみると、これは二重否定シフトとなる。二重否定シフトで二重否定シフトを証明しているという状況になってしまっているのだ。しかし、それでも限定継続を使って計算可能な形で実装している以上は、何らかの実体があるはずである。その実体というのは第 3 節に定義がある変換で見ることが出来る。

```txt
type_00 = [[ ((x : A) -> ((B x -> C) -> C)) -> (((x : A) -> B x) -> C) -> C ]]

type_01 = ([ ((x : A) -> ((B x -> C) -> C)) -> (((x : A) -> B x) -> C) -> C ] -> T) -> T

type_02 : (([ (x : A) -> ((B x -> C) -> C) ] -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_03 = ((((x : A) -> [[ ((B x -> C) -> C) ]]) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_04 = ((((x : A) -> ([ (B x -> C) -> C ] -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_05 = ((((x : A) -> (([ B x -> C ] -> [[ C ]]) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_06 = ((((x : A) -> ((([ B x ] -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_07 = ((((x : A) -> (((B x -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_08 = ((((x : A) -> (((B x -> ([ C ] -> T) -> T) -> [[ C ]]) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_09 = ((((x : A) -> (((B x -> (C -> T) -> T) -> [[ C ]]) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_10 = ((((x : A) -> (((B x -> (C -> T) -> T) -> ([ C ] -> T) -> T) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_11 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> [[ (((x : A) -> B x) -> C) -> C ]]) -> T) -> T

type_12 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> ([ (((x : A) -> B x) -> C) -> C ] -> T) -> T) -> T) -> T

type_13 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (([ ((x : A) -> B x) -> C ] -> [[ C ]]) -> T) -> T) -> T) -> T

type_14 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> ((([ (x : A) -> B x ] -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> T) -> T

type_15 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> [[ B x ]]) -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> T) -> T

type_16 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> ([ B x ] -> T) -> T) -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> T) -> T

type_17 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> [[ C ]]) -> [[ C ]]) -> T) -> T) -> T) -> T

type_18 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> ([ C ] -> T) -> T) -> [[ C ]]) -> T) -> T) -> T) -> T

type_19 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> (C -> T) -> T) -> [[ C ]]) -> T) -> T) -> T) -> T

type_20 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> (C -> T) -> T) -> ([ C ] -> T) -> T) -> T) -> T) -> T) -> T

type_21 = ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> T) -> T
```

実装も同様に変換することが出来る。

```txt
goal_00 = [ \ x -> \ y -> reset (y (\ z -> shift (\ w -> x z w))) ]

goal_01 = \ k_0 -> k_0 (\ x -> \ k_1 -> [ \ y -> reset (y (\ z -> shift (\ w -> x z w))) ] (\ t_0 -> k_1 t_0))

goal_02 = \ k_0 -> k_0 (\ x -> \ k_1 -> (\ k_2 -> k_2 (\ y -> \ k_3 -> [ reset (y (\ z -> shift (\ w -> x z w))) ] (\ t_1 -> k_3 t_1))) (\ t_0 -> k_1 t_0))

goal_03 = \ k_0 -> k_0 (\ x -> \ k_1 -> (\t_0 -> k_1 t_0) (\ y -> \ k_3 -> [ reset (y (\ z -> shift (\ w -> x z w))) ] (\ t_1 -> k_3 t_1)))

goal_04 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> [ reset (y (\ z -> shift (\ w -> x z w))) ] (\ t_1 -> k_3 t_1)))

goal_05 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> (\ k_4 -> k_4 ([ y (\ z -> shift (\ w -> x z w)) ] (\ t_2 -> t_2))) (\ t_1 -> k_3 t_1)))

goal_06 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> (\ t_1 -> k_3 t_1) ([ y (\ z -> shift (\ w -> x z w)) ] (\ t_2 -> t_2))))

goal_07 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ([ y (\ z -> shift (\ w -> x z w)) ] (\ t_2 -> t_2))))

goal_08 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ k_4 -> [ y ] (\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> k_4 t_5)))) (\ t_2 -> t_2))))

goal_09 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ([ y ] (\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> (\ t_2 -> t_2) t_5))))))

goal_09 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ([ y ] (\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> t_5))))))

goal_10 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ k_5 -> k_5 y) (\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> t_5))))))

goal_11 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> t_5))) y)))

goal_12 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ t_3 -> [ \ z -> shift (\ w -> x z w) ] (\ t_4 -> t_3 t_4 (\ t_5 -> t_5))) y)))

goal_13 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ([ \ z -> shift (\ w -> x z w) ] (\ t_4 -> y t_4 (\ t_5 -> t_5)))))

goal_14 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ k_6 -> k_6 (\ z -> \ k_7 -> [ shift (\ w -> x z w) ] (\ t_6 -> k_7 t_6))) (\ t_4 -> y t_4 (\ t_5 -> t_5)))))

goal_15 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 ((\ t_4 -> y t_4 (\ t_5 -> t_5)) (\ z -> \ k_7 -> [ shift (\ w -> x z w) ] (\ t_6 -> k_7 t_6)))))

goal_16 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> [ shift (\ w -> x z w) ] (\ t_6 -> k_7 t_6)) (\ t_5 -> t_5))))

goal_17 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> (\ k_8 -> let w = \ t_8 -> \ k_9 -> k_9 (k_8 t_8) in [ x z w ] (\ t_7 -> t_7)) (\ t_6 -> k_7 t_6)) (\ t_5 -> t_5))))

goal_18 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 ((\ t_6 -> k_7 t_6) t_8) in [ x z w ] (\ t_7 -> t_7)) (\ t_5 -> t_5))))

goal_19 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in [ x z w ] (\ t_7 -> t_7)) (\ t_5 -> t_5))))

goal_20 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in [ x z w ] (\ t_7 -> t_7)) (\ t_5 -> t_5))))

goal_21 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in (\ k_10 -> [ x z ] (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> k_10 t_11)))) (\ t_7 -> t_7)) (\ t_5 -> t_5))))

goal_22 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in [ x z ] (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> (\ t_7 -> t_7) t_11)))) (\ t_5 -> t_5))))

goal_23 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in [ x z ] (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_24 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in (\ k_10 -> [ x ] (\ t_12 -> t_12 z k_10)) (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_25 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in (\ k_10 -> (\ k_11 -> k_11 x) (\ t_12 -> t_12 z k_10)) (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_26 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in (\ k_10 -> (\ t_12 -> t_12 z k_10) x) (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_27 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in (\ k_10 -> x z k_10) (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_28 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in x z (\ t_9 -> [ w ] (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_29 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in x z (\ t_9 -> (\k_12 -> k_12 w) (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)))) (\ t_5 -> t_5))))

goal_30 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in x z (\ t_9 -> (\ t_10 -> t_9 t_10 (\ t_11 -> t_11)) w)) (\ t_5 -> t_5))))

goal_31 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> let w = \ t_8 -> \ k_9 -> k_9 (k_7 t_8) in x z (\ t_9 -> t_9 w (\ t_11 -> t_11))) (\ t_5 -> t_5))))

goal_32 = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))))
```

纏めると、次のようになる。

```txt
double_negation_shift : ((((x : A) -> (((B x -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> (((((x : A) -> (B x -> T) -> T) -> (C -> T) -> T) -> (C -> T) -> T) -> T) -> T) -> T) -> T
double_negation_shift = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))))
```

Coq で型が合っているか検証する。

```txt
Definition double_negation_shift (A : Type) (B : A -> Type) (C : Type) (D : Type) : (((forall x : A, (((B x -> (C -> D) -> D) -> (C -> D) -> D) -> D) -> D) -> ((((forall x : A, (B x -> D) -> D) -> (C -> D) -> D) -> (C -> D) -> D) -> D) -> D) -> D) -> D.
Proof.
intro k_0.
apply k_0.
intro x.
intro k_1.
apply k_1.
intro y.
intro k_3.
apply k_3.
Fail apply y.
Abort.
```

型が合わない。これは `C` と `D` が等しくないためだったので、 `C` と `D` が等しいと仮定する。

```txt
Definition double_negation_shift (A : Type) (B : A -> Type) (C : Type) : (((forall x : A, (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> ((((forall x : A, (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> C) -> C.
Proof.
intro k_0.
apply k_0.
intro x.
intro k_1.
apply k_1.
intro y.
intro k_3.
apply k_3.
apply y.
-
intro z.
intro k_7.
apply (x z).
intro t_9.
applu t_9.
+
intro t_8.
intro k_9.
apply k_9.
apply k_7.
exact t_8.
+
intro t_11.
exact t_11.
-
intro t_5.
exact t_5.
Defined.
```

これでは定義が複雑すぎるため、もう少し定義を省略できないか試みる。

```txt
double_negation_shift : ((((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> C) -> C
double_negation_shift = \ k_0 -> k_0 (\ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ k_1 -> k_1 (\ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5)))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5)

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11)))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8))))

double_negation_shift : ((x : A) -> (((B x -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> k_7 t_8)))

double_negation_shift : ((x : A) -> (((B x -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> k_7 t_8)))

double_negation_shift : ((x : A) -> (B x -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z (\ t_8 -> k_7 t_8))

double_negation_shift : ((x : A) -> (B x -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> \ k_7 -> x z k_7)

double_negation_shift : ((x : A) -> (B x -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> C) -> C
double_negation_shift = \ x -> \ y -> y (\ z -> x z)

double_negation_shift : ((x : A) -> B x) -> (((x : A) -> B x) -> C) -> C
double_negation_shift = \ x -> \ y -> y x

double_negation_shift : ((x : A) -> B x) -> (x : A) -> B x
double_negation_shift = \ x -> x
```

恒等関数となった。そういえば、継続モナドは簡略化していくと普通の関数になっちゃうんだった。どこかで簡略化を止めないといけない。

```txt
double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))
```

`reset` 関数と `shift` 関数の形が消えない所で止めることにする。そうなると、やっぱり、この辺りだろうか？

```
reset : ((A -> A) -> A) -> (A -> B) -> B
reset = \ (x : (A -> A) -> A) -> \ (y : A -> B) -> y (x (\ (z : A) -> z))
```

`reset` 関数は、この形をしている。

```txt
double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> \ k_3 -> k_3 (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))) (\ t_5 -> t_5))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> reset (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))))
```

`reset` 関数を使う形に書き換える。

```txt
shift : ((A -> (B -> B) -> B) -> (B -> B) -> B) -> (A -> B) -> B
shift = \ (x : (A -> (B -> B) -> B) -> (B -> B) -> B) -> \ (y : A -> B) -> x (\ (z : A) -> \ (w : B -> B) -> w (y z)) (\ (v : B) -> v)
```

`shift` 関数は、この形をしている。

```txt
double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> reset (y (\ z -> \ k_7 -> x z (\ t_9 -> t_9 (\ t_8 -> \ k_9 -> k_9 (k_7 t_8)) (\ t_11 -> t_11))))

double_negation_shift : ((x : A) -> (((B x -> (C -> C) -> C) -> (C -> C) -> C) -> C) -> C) -> (((x : A) -> (B x -> C) -> C) -> (C -> C) -> C) -> (C -> C) -> C
double_negation_shift = \ x -> \ y -> reset (y (\ z -> shift (\ w -> \ v -> x z (\ u -> u w v))))
```

`shift` 関数を使う形に書き換える。

"[Delimited control operators prove Double-negation Shift](https://arxiv.org/abs/1012.0929)" は、要するに、全ての関数に継続の文脈が乗っていることを前提にしているわけだ。では、 Labda の全ての関数に継続の文脈が乗っていることにすればよいかというと、それは難しい。一つ目に、それでは構成的な議論と非構成的な議論の分離が出来なくなる。二つ目に、関数そのものの内容が型を変え得るようなフルの依存型を含む理論に継続を乗せることは大変難しい。三つ目に、 HoTT の公理と矛盾してしまう。

Labda ではプリミティブとし、それをラカーセアーへ翻訳する際に実体を持たせる形はどうだろうか？　これも依存型と HoTT の公理との融合が問題となる。だが、 Labda そのものに手を入れるよりは希望がありそうである。また、実行順序を規定しているため継続を使っても値が不定にならないことも追い風になる。

ここまでは `Continue T A = ((A -> T) -> T)` という文脈のもとで古典論理をエミュレーションしようとしていたが、これをプリミティブとするとなると、その名前を普通の継続モナドと被らないようにしなければならない。そこで、 Classic モナドとすることにする。

一つの試案として、 `Classic A = ((A -> Any) -> Any)` と定義することを考えている。そして、排中律と二重否定シフトを持ち、 `Unwrap : Classic Type -> Type` と `(x : Classic A) -> (y : (x : A) -> Classic (B x)) -> Unwrap (bind x B)` を持つとする。だが、これはたぶんうまくいかない。

### Labda は等式型を持つものとする。

等式型 (identity type) は、定理証明支援系としての能力を得るために必要である。

### Labda は三種類の等式型を持つものとする。

等式型の定義は、様々なものがある。ライプニッツの定義もあれば、帰納的な定義もあるし、立方型理論 (cubical type theory) では関数に類似したものとして定義される。

また、公理 K (axiom K) が成り立つかどうか、あるいは一価性公理が成り立つかどうかもある。

さらに、プログラミング言語として見なすと、等式型を元にして変形した時に、コストが掛かってしまう可能性があると直感的ではないという問題もある。

これらの問題を全て捉えるために、 Labda は三種類の等式型を持つものとする。

一つ目は、通常の MLTT における帰納的な等式型である。この等式型は、最も厳密なものである。

二つ目は、内部表現が等しいためゼロコストでお互いに変換できる等式型である。これは、 Haskell の newtype 機能を使った時に発生する、ラッパーを付け外しするだけで変換できるような場合を表す。

三つ目は、 HoTT における道型である。この等式型に関しては、関数外延性 (functional extensionality) と一価性公理 (univalence axiom) が成り立つ。その代わり、ホモトピー命題ではないし、この型を元にして変換するのにはコストがかかる。

### ラカーセアーはグラフ簡約が可能である。

グラフの辺は向きを持つ。 `A` から `B` への矢印があるとき、 `A` の計算には `B` の計算が必要であることを示す。それぞれの辺は、必ず一つの辺と繋がる。

`x` は次のようになる。

```txt
  | ^
  | |
  | | x
  v |
+-----+
| var |
+-----+
```

`t $ s` は次のようになる。

```txt
    |
    |
    |
    v
+-------+
|  app  |
+-------+
  |   |
  |   |
  |   |
  v   v
```

`lambda x then t` は次のようになる。

```txt
    |
    |
    |
    v
+-------+
|  lam  |
+-------+
  ^   |
  |   |
x |   |
  |   v
```

`dup y, z := x then t` は次のようになる。

```txt
   |   ^
 x |   |
   |   |
   v   |
+---------+
|   dup   |
+---------+
  ^   ^ |
  |   | |
y | z | |
  |   | v
```

`del x then t` は次のようになる。

```txt
  ^ |
  | |
x | |
  | v
+-----+
| del |
+-----+
   |
   |
   |
   v
```
