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


### ラカーセアーはメモリの確保と破棄のタイミングをコントロールできるものとする。

ラムダ計算を実際に実行することを考えると、たとえば、 `( \ x_ -> t ) x` を簡約する時に、 `t` の中で `x_` が 2 箇所以上で現れているとすると、 `x` を複製しなければならない。 `t` の中に `x_` が現れていないとすると、 `x` を破棄しなければならない。

では、どのようなタイミングで複製と破棄をすればよいのだろうか？　最後の最後まで複製をしないことも、出来る限り早いタイミングで複製することも出来る。

普通のプログラミング言語なら、それをコンパイラに任せるのが普通である。しかし、ラカーセアーは、 Labda のコンパイル先となる言語であり、普段は隠れているものをコントロールできるようにするというのがコンセプトなのである。
