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

### Labda は古典論理を記述できる。

Labda は、高度な数学を記述できる。今までの数学は、そのほとんどが古典論理によるものであり、高度な数学を記述するならば古典論理を記述できる必要がある。

このため、 Labda は古典論理を記述できる。

### Labda は同値性の型を持つ。

Labda は、高度な数学を記述できる。同値性の型は、高度な数学を記述するために必須である。

このため、 Labda は同値性の型を持つ。

### Labda は三種類の等式型を持つものとする。

等式型の定義は、様々なものがある。ライプニッツの定義もあれば、帰納的な定義もあるし、立方型理論 (cubical type theory) では関数に類似したものとして定義される。

また、公理 K (axiom K) が成り立つかどうか、あるいは一価性公理が成り立つかどうかもある。

さらに、プログラミング言語として見なすと、等式型を元にして変形した時に、コストが掛かってしまう可能性があると直感的ではないという問題もある。

これらの問題を全て捉えるために、 Labda は三種類の等式型を持つものとする。

一つ目は、通常の MLTT における帰納的な等式型である。この等式型は、最も厳密なものである。

二つ目は、内部表現が等しいためゼロコストでお互いに変換できる等式型である。これは、 Haskell の newtype 機能を使った時に発生する、ラッパーを付け外しするだけで変換できるような場合を表す。

三つ目は、 HoTT における道型である。この等式型に関しては、関数外延性 (functional extensionality) と一価性公理 (univalence axiom) が成り立つ。その代わり、ホモトピー命題ではないし、この型を元にして変換するのにはコストがかかる。

### Labda は現代の数学を実装するために十分な機能を持つ。

一つ目に、代数的データ型と等式型により一階述語論理を表現することが可能である。二つ目に、帰納型と余帰納型により自然数などを表現することが可能である。三つ目に、宇宙により高階述語論理を表現することが可能である。四つ目に、高階帰納型により商集合などを表現することが可能である。五つ目に、多段階計算によりゲーデルの第二不完全性定理の証明で使うようなメタな議論を表現することが可能である。

## ラカーセアーの設計

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

ファイル操作やインターネット接続などを実装するためには、副作用が必須である。副作用をコントロールするための手法として IO モナドなどがある。しかし、 Labda のコンパイル先がラカーセアーであるため、ラカーセアーの仕様は極々単純にしなければならない。そのためにラカーセアーは型なしラムダ計算を基本としている。 IO モナドには型が必要であるため、 IO モナドを使うことは出来ない。そのため、ラカーセアーは、それを簡約した際に副作用が発生する関数を持つものとする。

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
