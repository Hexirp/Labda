# Labda

Labda はプログラミング言語かつ定理証明支援システムである。

## 名前の由来

"Labda" は "lambda" の古い語形が由来である。

## 基本設計

### Labda はプログラミング言語かつ定理証明支援システムである。

私は、数学を精密な人工言語で記述するということに魅力を感じている。 Coq を頻繁に用いているが、いくつかの不満も感じている。その一方で、私は、プログラムの正確性を数学で保証するということに魅力を感じている。しかし、高度な数学も記述でき、高度なプログラムも記述でき、私が求めるような機能も全て備えているような人工言語は、いまだない。

そのため、 Labda はプログラミング言語かつ定理証明支援システムである。

### Labda はプログラムの効率性を保証する機能を持つ。

プログラミング言語は、コンピューターへの命令を、より効率よく、より正確に、より分かりやすく、より変更しやすく、記述するためにある。プログラムのコードが複雑になるほど、その内部へ余計な計算をしたり余計な領域を使ったりするようなコードが入り込みやすくなる。そのようなコードを防ぐ機能が必要である。

そのため、 Labda はプログラムの効率性の記述を保証する機能を持つ。

### Labda はプログラムの正確性を保証する機能を持つ。

プログラミング言語は、コンピューターへの命令を、より効率よく、より正確に、より分かりやすく、より変更しやすく、記述するためにある。プログラムのコードが大きくなるほど、その内部へ間違いが入り込みやすくなる。そのような間違いを検出する機能が必要である。

そのため、 Labda はプログラムの正確性を保証する機能を持つ。

### Labda はプログラムのコードの可読性を高める機能を持つ。

プログラミング言語は、コンピューターへの命令を、より効率よく、より正確に、より分かりやすく、より変更しやすく、記述するためにある。プログラムのコードが大きくなるほど、それは理解しづらくなる。そのような難しさを軽減する機能が必要である。

そのため、 Labda はプログラムの可読性を高める機能を持つ。

### Labda はプログラムのコードの拡張性を高める機能を持つ。

プログラミング言語は、コンピューターへの命令を、より効率よく、より正確に、より分かりやすく、より変更しやすく、記述するためにある。プログラムのコードが大きくなるほど、それは変更しづらくなる。そのような難しさを軽減する機能が必要である。

そのため、 Labda はプログラムの拡張性を高める機能を持つ。

### Labda はラムダ計算を基礎とする。

プログラミング言語は、「それをどのようにして実行するのか」が定まっている必要がある。そのため、どのような計算モデルを採用するのかを考えなければならない。 Labda はプログラミング言語なのでチューリング完全であるべきである。そのような計算モデルには様々な種類がある。たとえば、コンビネータ論理や、ラムダ計算や、チューリングマシンや、レジスタマシンや、項書き換えシステムや、 μ-再帰関数や、セルオートマトンなどである。

私は、これらの中からラムダ計算を選ぶ。その理由は、七つある。

一つ目に、あるプログラムのコードを書いたとき、そのコードを複数のコードへと分けて捉えることが容易であり、それぞれのコードをどのように実行しても、その結果は常に同じであり、それが他のコードの実行結果へと影響することがない。あるコードが安全であることを、他のコードを考慮せずに保証できるということであり、安全性が高い。あるコードが他のコードと干渉し合わないための規約を定める必要がなく、可読性が高い。あるコードを変更しても、その結果を変えなければ他のコードへ影響を及ぼさないため、拡張性が高い。

二つ目に、「名前」を持つ。プログラミング言語は、通常の場合、バッカスナウア記法を用いて定義することができる。しかしながら、自然言語は、生成文法に従う入れ子の構造を基本としているというわけではなく、むしろ小さな部品を平坦に連結して並べていることがほとんどであるように、私は思う。そして、人間が読みやすいプログラムのコードというのも、このような自然言語と同様に、できるだけ入れ子の構造を避けて小さな部品を平坦に連結して並べるという特徴を持っているように、私は思う。このように考えて、小さな部品を平坦に連結して並べることが人間が親しみやすい言語の本質であるのではないかと、私は思う。だが、小さな部品を平坦に連結して並べるだけでは多様な処理を表現することはできない。つまり、まだ足りない要素がある。それは「名前」なのではないかと私は思う。「名前」を使うことにより、多様多種な概念の網のような繋がりを表せるようになると私は思う。このように、「名前」は人間の思考において重要であると私は思う。

三つ目に、型を追加しやすい。そのコードを複数のコードへと分けて捉えることができるため、それらの複数のコードについて、それぞれの型を考え、それから全体のコードについて型を考えることができる。それぞれの部分に型を付けることができ、そこから全体に型を付けることができる。「名前」を持つため、その名前へ関連づけて型を扱うことができる。いくつかの文法を追加するだけで「型」も表現できるようになり、新しく「型」のための表記を定める必要がない。型と値の表記がほぼ同じであるため、値を型へ埋め込むことが容易であり、依存型も表現しやすく、高度な数学を表現できるようになる。

四つ目に、他の計算モデルであっても、適切にプログラムのコードを複数の部品へ分割しようとすると、自ずからと「ラムダ抽象」と「適用」と同等の概念を用いることになり、ただ「ラムダ抽象」と「適用」へ複雑な制約を加えただけのものになり、最初から「ラムダ抽象」と「適用」だけを扱う方がよくなる。

五つ目に、プログラム自体を値として扱え、どのような部分も関数として抜き出せるため、非常に表現力が高い。これにより、プログラムからプログラムを作るプログラムというような高階関数まで表現することができる。

六つ目に、人間でも分かりやすいプログラムのコードを書くのが最も簡単な計算モデルである。特にリストなどを容易なエンコーディングで扱うことができる。

七つ目に、 Coq と Agda と Lean と Idris はラムダ計算を基本としている。

このため、 Labda はラムダ計算を基礎とする。

### Labda は構成主義を採用する。

Labda は、定理証明支援システムであり、プログラミング言語であり、その両方でもある。そのため、 Labda でのプログラムのコードは Labda での定理の証明として読めるべきだし、 Labda での定理の証明は Labda でのプログラムのコードとして読めるべきである。これは、構成主義である。（これは本当なのだろうか？）

このため、 Labda は構成主義を採用する。

### Labda の意味論は Labda のコードを型なしラムダ計算の式に変換する写像で定める。

Labda はラムダ計算を基礎としている。しかし、ただの型なしラムダ計算に収まらず、その範囲を超えた様々な機能を持つ。たとえば、帰納型や余帰納型などのような機能のことである。しかしながら、これらの機能の中には、時として型なしラムダ計算ではないかのような動作を実現する。 Labda はラムダ計算を基礎にしていると言い切るため、そのような機能も型なしラムダ計算で理解できることを示す必要がある。

そのため、 Labda の意味論は Labda のコードを型なしラムダ計算の式に変換する写像で定める。

### Labda はラカーセアーを基礎とする。

型なしラムダ計算は、計算を数学で扱うための計算モデルとして、充分な能力を備えている。しかし、プログラミング言語の基礎として使うためには、いくつか欠けている機能がある。

1. 外部とのやりとり
1. 実行順序のコントロール
1. メモリの確保と解放のタイミングのコントロール

そのような機能を持つラムダ計算を、「其の副作用が明示的なラムダ計算」と呼ぶことにする。「其の副作用が明示的なラムダ計算」は長すぎる。そのため、それを英訳した "lambda calculus whose side effects are explicit" を縮めた "lacawseae" を音写した「ラカーセアー」とする。

Labda は、現実のプログラムも記述できる必要がある。つまり、「外部とのやりとり」と「実行順序のコントロール」と「メモリの確保と解放のタイミングのコントロール」が可能な必要がある。

そのため、 Labda はラカーセアーを基礎とする。

### Labda は低水準な領域のコードも書ける。

Labda はプログラミング言語である。より効率的なコードを書くためには、時として低水準な領域のコードを書く必要があることもある。

そのため、 Labda は低水準な領域のコードも書ける。

### Labda は非安全なコードも書ける。

Labda は低水準な領域のコードも書ける。しかし、その安全性を保証することは難しい。また、安全性と効率性のトレードオフがある場合もある。

そのため、 Labda は非安全なコードも書ける。

### ラカーセアーのコードを Labda のコードへ埋め込める。

Labda は非安全で低水準な領域のコードも書ける。 Labda の非安全で低水準な領域というのはラカーセアーである。よって、 Labda の中でラカーセアーを書ける必要がある。

そのため、ラカーセアーのコードを Labda のコードへ埋め込める。

### Labda は安全なコードと非安全なコードを区別する機能を持つ。

Labda は非安全で低水準な領域のコードも書けるが、それが安全なコードと混じり合うと、安全性を保証することが出来なくなる。

そのため、 Labda は安全なコードと非安全なコードを区別する機能を持つ。

### Labda で非安全なコードを書く方法はラカーセアーのコードを埋め込むことのみである。

Labda の非安全で低水準な領域というのはラカーセアーである。そして、そのラカーセアーのコードを Labda のコードへ埋め込むことで非安全なプログラムのコードを書くことが出来る。ならば、そのような非安全なコードを書く方法を、ラカーセアーのコードを埋め込むことのみに限ることで、非安全な側面をラカーセアーへ閉じ込め、分かりやすくすることが出来る。

そのため、 Labda で非安全なコードを書く方法はラカーセアーのコードを埋め込むことのみである。

### Labda は、ほとんどのプログラムの分野で使うことができる。

Labda は、高度なプログラムを書くことができ、その正しさを検証することができる。高度なプログラムというのは、往々にして分野を跨ぐ必要がある。そのような部分も、一元的に Labda で検証したい。

そのため、 Labda は、ほとんどのプログラムの分野で使うことができる。

### Labda は実行可能ファイルを生成することができる。

Labda は、高度なプログラムを書くことができる。それには、もちろんアプリケーションも含むべきである。

そのため、 Labda は実行可能ファイルを生成することができる。

### Labda は実行順序をコントロールできる機能を持つ。

Labda はラムダ計算を基礎としている。ラムダ計算は、項書き換えシステムとして見なすと、実行順序の曖昧さが出てくる。それが、次の二つである。

1. ラムダ抽象の項において、その内側の項も簡約するかどうか。つまり、 $` \lambda x \ldotp t `$ の内側の $` t `$ を簡約するかどうか。
1. 適用の項において、その関数の部分の項と、その引数の部分の項の、どちらを先に簡約するかどうか。つまり、 $` f \cdot x `$ において、 $` f `$ から簡約をするのか、 $` x `$ から簡約をするのか。

Labda でも、この曖昧さは問題となる。要するに、これらの実行順序をどうするかにより言語の仕様が変わる。

一つ目の選択肢について、つまり「ラムダ抽象の項において、その内側の項も簡約するかどうか」については、私が知る限り、これを行う言語はない。しかしながら、ラムダ計算を基本とし、チャーチエンコーディングを使い、列挙型の内側まで評価したいとなると、必然的に「ラムダ抽象の項において、その内側の項も簡約する」ことを許す必要がある。

例えば、 `Sum A B` を $` \lambda a \ldotp \lambda f \ldotp \lambda g \ldotp f \cdot a `$ と $` \lambda b \ldotp \lambda f \ldotp \lambda g \ldotp g \cdot b `$ の種別を持つ列挙型として、これを通常のプログラミング言語と同様に評価しようとすると、必然的に $` f `$ と $` g `$ のラムダ抽象を飛び越えて $` a `$ または $` b `$ を評価することができることになる。

その一方で、余帰納型をエンコーディングする場合など、その内側を評価できなくしてほしいようなラムダ抽象もある。このため、どちらかを選べるようにして、プログラマーへ決定権を委ねる。

二つ目の選択肢について、つまり「適用の項において、その関数の部分の項と、その引数の部分の項の、どちらを先に簡約するかどうか」については、一つ目の選択肢よりも複雑な事情がある。どちらを先に簡約することにしても、それぞれのメリットとデメリットがある。

引数の部分の項を最初に簡約することにすると、評価の順番が理解しやすくなり、メモリの確保と解放のタイミングも理解しやすくなる。その一方で、余帰納型を表現することができなくなり、 let 式の項をそのまま適用の項に翻訳することができなくなり、短絡評価を実現するために特別扱いが必要となり、余分な項まで簡約してしまう。

関数の部分の項を最初に簡約することにすると、余分な項まで簡約してしまうことがなくなり、評価不要な項を評価しないまま捨てることが簡単にできるようになる。その一方で、評価の順番が理解しづらく、メモリの確保と解放のタイミングも理解しづらく、適切に取り扱わないと未評価の項が膨らんでメモリを食い潰すようになる。

このように、どちらを先に簡約することにしても、それぞれのメリットとデメリットがある。そのため、どちらも平等に扱うものとする。つまり、実行順序をコントロールできるようにして、プログラマーへ決定権を委ねる。

このため、 Labda は実行順序をコントロールできる機能を持つ。

### Labda はメモリの確保と解放のタイミングをコントロールできる機能を持つ。

Labda は高度なプログラムを書くことができるプログラミング言語である。そうであるならば、メモリの確保と解放のタイミングをコントロールできる必要がある。

そのため、 Labda はメモリの確保と解放のタイミングをコントロールできる機能を持つ。

### Labda は外部とのやり取りをコントロールできる機能を持つ。

Labda は様々な場面で使用できるプログラミング言語である。そうであるならば、外部とのやり取りをコントロールできる機能を持たなければならない。

そのため、 Labda は外部とのやり取りをコントロールできる機能を持つ。

### Labda は外部とのやりとりを作用子でコントロールする。

純粋なラムダ計算は、全く作用を持たない。しかしながら、これに作用を加えることができる。それは、次のような変換を行うということである。

1. 変数を持ち上げることができる。 $` x : A `$ とする。 $` x : A `$ を変換すると $` [ x ] : F \cdot A `$ であり、これを進めると $` \mathrm{pure} \cdot x : F \cdot A `$ である。
1. 適用を持ち上げることができる。 $` f : A \rightarrow B `$ とする。 $` x : A `$ とする。 $` f \cdot x : B `$ を変換すると $` [ f \cdot x ] : F \cdot B `$ であり、これを進めると $` ( \mathrm{bind} \cdot [ f ] ) \cdot [ x ] : F \cdot B `$ である。
1. ラムダ抽象を持ち上げることができる。 $` x : A \vdash t : B `$ とする。 $` \lambda x \ldotp t : A \rightarrow B `$ を変換すると $` [ \lambda x \ldotp t ] : A \rightarrow F \cdot B `$ である。これを進めると、 $` \lambda x \ldotp [ t ] : A \rightarrow F \cdot B `$ である。

ここで $` \mathrm{pure} `$ と $` \mathrm{bind} `$ という二種類の関数が出てきた。これらの型は、 $` x : A \vdash \mathrm{pure} \cdot x : F \cdot A `$ と $` f : A \rightarrow F \cdot B , x : F \cdot A \vdash ( \mathrm{bind} \cdot f ) \cdot x : F \cdot B `$ である。これらは、三種類の則を持つ。

1. $` f : A \rightarrow F \cdot B , x : A \vdash ( \mathrm{bind} \cdot f ) \cdot ( \mathrm{pure} \cdot x ) \equiv f \cdot x : F \cdot B `$ である。
1. $` x : F \cdot A \vdash ( \mathrm{bind} \cdot \mathrm{pure} ) \cdot x \equiv x : F \cdot A `$ である。
1. $` g : B \rightarrow F \cdot C , f : A \rightarrow F \cdot B , x : F \cdot A \vdash ( \mathrm{bind} \cdot \lambda y \ldotp ( \mathrm{bind} \cdot g ) \cdot ( f \cdot y ) ) \cdot x \equiv ( \mathrm{bind} \cdot g ) \cdot ( ( \mathrm{bind} \cdot f ) \cdot x ) : F \cdot C `$ である。

このような性質を持つ $` F `$ を作用子と呼ぶことにする。実のところ、これは Haskell がモナドと呼ぶものである。このような作用子は、ラムダ計算と親和性が高いので、これを利用して外部とのやりとりをコントロールするものとする。

他の選択肢として、 Haskell でいうコモナドとアローがある。アローはラムダ計算へ親和しないため、すぐに除外することができる。コモナドは、モナドと同様にラムダ計算と親和するが、ラムダ計算が直観主義論理に対応するがゆえに、ラムダ計算の関数において始域と終域は非対称であり、それがゆえにモナドと比べてコモナドの表現力は低くなるため、これも除外するものとする。そうすると、モナドだけが選択肢に残る。

このため、 Labda は作用子で外部とのやりとりをコントロールする。

### Labda は非安全なコードを作用子で区別する。

Labda は安全なコードと非安全なコードを区別する機能を持つ。ここで、安全なコードと安全なコードを合成したのは安全なコードであること、安全なコードと非安全なコードを合成したのは非安全なコードであること、非安全なコードと非安全なコードを合成したのは非安全なコードであること、これらに注意すると、これは作用子で取り扱えることに気づく。

Labda で非安全なコードを書く方法はラカーセアーのコードを埋め込むことのみである。このため、ラカーセアーを埋め込むことができる作用子を使い、非安全なコードを管理することになる。では、どのような作用子であればラカーセアーを埋め込むことができるのだろうか？

ラカーセアーは型なしラムダ計算である。これを型付きラムダ計算として見なすと、それは $` T \equiv ( T \rightarrow T ) `$ が成り立つような型 $` T `$ だけを持つ型付きラムダ計算となる。これは、ありとあらゆる命題を証明できるような理論である。であれば、任意の型 $` A `$ に対して $` F \cdot A `$ が存在すればよいのだろうか？ いや、これでは Maybe モナドと同じになる。

型なしラムダ計算は、それが持つ型が全て $` T \equiv ( T \rightarrow T ) `$ が成り立つような型 $` T `$ と等しくなる。それは、任意の型 $` A `$ に対して $` A \rightarrow T `$ と $` T \rightarrow A `$ が存在するということである。 さらに、特別な型を導入するのは作用子の中では実現できないため、これらを合成して任意の型 $` A `$ と任意の型 $` B `$ に対して $` A \rightarrow B `$ が存在することにする。これを作用子に埋め込むと、 $` A \rightarrow F \cdot B `$ となる。これは、 Haskell の `unsafeCoerce` のような操作である。

このため、 Labda は非安全なコードを作用子で区別する。

### Labda は古典論理を記述できる。

Labda は、高度な数学を記述できる。今までの数学は、そのほとんどが古典論理によるものであり、高度な数学を記述するならば古典論理を記述できる必要がある。

このため、 Labda は古典論理を記述できる。

### Labda は命題論理の論理演算子に対応する型を持つ。

Labda は、高度な数学を記述できる。そのためには、命題論理の論理演算子が記述できる必要がある。

このため、 Labda は命題論理の論理演算子に対応する型を持つ。

### Labda は一階述語論理の量化子に対応する型を持つ。

Labda は、高度な数学を記述できる。そのためには、一階述語論理の量化子が記述できる必要がある。

このため、 Labda は一階述語論理の量化子に対応する型を持つ。

### Labda は帰納型を持つ。

Labda は、高度な数学を記述できる。そのためには、自然数を定義できる必要がある。

Labda は、高度なプログラムを記述できる。そのためには、帰納型が必要である。

このため、 Labda は帰納型を持つ。

### Labda は余帰納型を持つ。

Labda は、高度な数学を記述できる。 Mike Shulman さんが 2022 年 2 月 9 日に投稿した文章（[リンク](https://proofassistants.stackexchange.com/questions/208/when-to-use-coinductive-types/210#210)）によると、 ∞-圏に関する概念を余帰納型で定義すると簡潔になる場合がある。

Labda は、高度なプログラムを記述できる。そのためには、余帰納型が必要である。たとえば、ミーリマシンなどである。

このため、 Labda は余帰納型を持つ。

### Labda は同値性の型を持つ。

Labda は、高度な数学を記述できる。同値性の型は、高度な数学を記述するために必須である。

このため、 Labda は同値性の型を持つ。

### Labda は宇宙を持つ。

Labda は、高度な数学を記述できる。そのためには、宇宙が必要である。

このため、 Labda は宇宙を持つ。

### Labda は高階帰納型を持つ。

Labda は、高度な数学を記述できる。そのためには、高階帰納型が必要である。

このため、 Labda は高階帰納型を持つ。

### Labda は多段階計算の機能を持つ。

Labda は、高度な数学を記述できる。ゲーデル数や強制法などのようなメタな議論を型理論で表記するためには、多段階計算が必要である。

このため、 Labda は多段階計算の機能を持つ。

### Labda は Homotopy Type Theory を取り入れる。

Homotopy Type Theory は、商型を適切に取り扱うためには必要である。

このため、 Labda は Homotopy Type Theory を取り入れる。

### Labda は Quantitative Type Theory を取り入れる。

Quantitative Type Theory は、依存型に対する適切な型消去を助け、線形論理と同等の原理でメモリの管理を可能にする。

このため、 Labda は Quantitative Type Theory を取り入れる。

### Labda は型クラスに類似する機能を持つ。

Haskell は型クラスでインターフェースの共通化を実現する。これは高度なプログラムを効率的かつ安全かつ分かりやすく変更しやすく記述するために必要な機能である。

このため、 Labda は型クラスに類似する機能を持つ。

### Labda は newtype に類似する機能を持つ。

Haskell は newtype に類似する機能を持つ。これは、その型を使う理由を分かりやすくし、その型では不正な操作を行うことを防止することができる。高度なプログラムを記述する助けになる。

このため、 Labda は newtype に類似する機能を持つ。

### Labda は変換コストがない同値性の型を持つ。

Homotopy Type Theory の同値性の型は、非常に強い性質を持ち、同型な型も同値だと見なすことができる。そして、その同値性の型を元にして、その片方の型の値を、もう片方の型の値へ移すことができる。しかし、そのためには大きな変換コストがかかることがあるため、プログラミング言語としては感覚に合わない。

そこで、 Labda は変換コストがない同値性の型を持つことにすればよい。この同値性の型は、変換コストを必要とせずに移り合うものを結ぶ。

このため、 Labda は変換コストがない同値性の型を持つ。

### Labda は簡約で移り合うだけの同値性の型を持つ。

Labda は型クラスに類似する機能を持つ。この機能は、変換コストがない場合でも、そのような型を区別する。そのため、型クラスに関する制約を書く時は、簡約で移り合うだけの同値性の型が必要になる。

このため、 Labda は簡約で移り合うだけの同値性の型を持つ。

### Labda は type role に類似する機能を持つ。

Labda は、複数の同値性の型を持つ。これらの同値性の型は、それぞれ $` a = b `$ である場合に $` p \cdot a \rightarrow p \cdot b `$ を生成することができる。このような操作が、どの場合に可能なのかを管理するために、 Haskell の type role のような機能が必要になる。

このため、 Labda は type role に類似する機能を持つ。

## ラカーセアーの設計

### ラカーセアーは外部とやりとりすることができる。

ラカーセアーは、「外部とのやりとり」と「実行順序のコントロール」と「メモリの確保と解放のタイミングのコントロール」の機能を追加した型なしラムダ計算である。

このため、ラカーセアーは外部とやりとりすることができる。

### ラカーセアーは実行順序をコントロールすることができる。

ラカーセアーは、「外部とのやりとり」と「実行順序のコントロール」と「メモリの確保と解放のタイミングのコントロール」の機能を追加した型なしラムダ計算である。

このため、ラカーセアーは実行順序をコントロールすることができる。

### ラカーセアーはメモリの確保と解放のタイミングをコントロールすることができる。

ラカーセアーは、「外部とのやりとり」と「実行順序のコントロール」と「メモリの確保と解放のタイミングのコントロール」の機能を追加した型なしラムダ計算である。

このため、ラカーセアーはメモリの確保と解放のタイミングをコントロールすることができる。

### ラカーセアーは評価の時に副作用を起こす値を持つ。

ラカーセアーは外部とやりとりすることができる。それを実現するためには、ラカーセアーの項を簡約する過程において、そのどれかで副作用を起こす必要がある。

そのため、ラカーセアーは評価の時に副作用を起こす値を持つ。

### ラカーセアーはアである。

ラカーセアーは実行順序をコントロールすることができる。では、それを実現するためには、どのような機能を加えればよいのだろうか？ 実際の使用例で考えてみよう。

Edward Kmett さんが 2021 年 1 月 31 日に投稿した文章（[リンク](https://www.reddit.com/r/haskell/comments/l98v73/comment/glgun65/)）と、 Edward Kmett さんが 2024 年 6 月 1 日に投稿した文章（[リンク](https://x.com/kmett/status/1796571830557212905)）によると、次の場合には遅延評価が有用である。

1. 純粋性を保ちながら不変データ構造を扱うと、特定の値だけを対象にして何回も同じ操作を繰り返して償却性を破壊することができるが、遅延評価を使うと償却性を回復することができる。ただし、線形論理を使えば純粋性を保ちながら可変データ構造を使用することができる。
1. 短絡評価を可能にする。制御構文の関数を実装することを可能にする。評価不要な項を評価しないまま捨てることが簡単にできる。再帰構造を評価しないまま定義することができる。
1. let 式を適用へ変換する時に、 let 式の中の定義を評価しないようにすることが簡単である。定義と適用が一致するようになる。

（執筆中）

### ラカーセアーは線形ラムダ計算である。

ラカーセアーはメモリの確保と解放のタイミングをコントロールすることができる。では、それを実現するためには、どのようにすればよいのだろうか？

ラムダ計算のメモリ管理を分かりづらくしているのは、ベータ簡約の時に、その引数の方の項を、いくらでも複製したり破棄したりすることができることである。であるならば、ラムダ抽象で導入した変数を必ず一回だけ消費するようにすることで、メモリ管理を明確にすることができる。

このため、ラカーセアーは線形ラムダ計算である。

### ラカーセアーは複製と破棄を特殊な構文で行う。

ラカーセアーは線形ラムダ計算である。しかしながら、単なる線形ラムダ計算では、分岐とループを表現することができない。複製と破棄が必要である。

最初の動機を振り返ってみると、ベータ簡約で複製と破棄が暗黙的に起こることが原因であった。ならば、複製と破棄を明示的に行う構文を導入すればよい。それは、たとえば `copy t to x and y then s` や `delete t then s` などのような構文である。これを使うと、最小の簡約が停止しない式は `( lambda x => copy x to y and z then y z ) ( lambda x => copy x to y and z then y z )` となる。

このため、ラカーセアーは複製と破棄を特殊な構文で行う。

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

## 開発方針

### 全体の枠組みを作ってから細部を広げていく。

くまぎさんが 2023 年 7 月 24 日 8 時 31 分に投稿した文章（[リンク](https://x.com/kumagi/status/1683258579166588928)）と、 mizchi さんが 2023 年 7 月 24 日 14 時 50 分に投稿した文章（[リンク](https://x.com/mizchi/status/1683353823400841217)）と、くまぎさんが 2023 年 7 月 24 日 15 時 24 分に投稿した文章（[リンク](https://x.com/kumagi/status/1683362569745231872)）と、 Fernando Borretti さんが 2022 年 7 月 5 日に公開した文章（[リンク](https://borretti.me/article/lessons-writing-compiler)）を読む。

これらによると、「ファイルを読み込むところ、パーサーのところ……」というように順番に作っていくと、ファイル読み込みやパーサーなどの仕様が揺らぎやすいため、それらに対するテストを変更する頻度が高くなりすぎる。その代わりに、入力の形式と出力の形式だけが定まっている非常に単純な機能だけを持つコンパイラーを作り、その後に機能を追加していき、入力と出力だけを対象にしてテストを行うことで、よりよく開発を進めることができる。

このため、全体の枠組みを作ってから細部を広げていく。
