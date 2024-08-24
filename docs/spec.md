# 言語仕様

## 文法

### プログラム

プログラムは0個以上の文(statement)から成り、先頭の文から順番に評価される。

#### If文

一般的な条件分岐

```text
if (flag) {
    print("true")
} else {
    print("false")
}
```

- else節は省略可能

#### For文

一般的な繰り返し処理

```text
let array = [0, 1, 2];

for (x in array) {
    print(x)
}
```

- 繰り返し処理本体は別スコープとなる
- ループ変数(上記コード例の`x`)は繰り返し処理本体のスコープ内で定義される
- iterableなオブジェクトの詳細はこの仕様では未定義

#### ブロック文

複数の文をまとめた文。

```text
{
    let x = 1
    x * 2
} 
```

- ブロック内は別スコープとなり、ブロック内で宣言された変数・関数・型は外部から参照できない

#### 変数定義

変数を定義する。

```text
let x:number = 1
let y = "hello"
let z:bool

z = true
```

- 変数の型、初期値は省略可能
    - 変数の型が省略された場合、初期値から型を推論する
    - 初期値が省略された場合、初期化前に変数を参照するとエラーになる

#### 関数定義

関数を定義する。

```text
fn double(x:number):number {
    return x * 2
}
```

- 引数の型は省略できない
- 戻り値の型は省略可能であり、省略した場合は戻り値がないことを示す
- 戻り値はreturn文で明示的に指定する必要がある

#### 構造体定義

構造体を定義する。

```text
struct User(id:number, name:string) {
    fn getId(self) {
        return self.id
    }
}
```

- 定義した構造体は関数呼び出しと同じ文法で初期化することができる

    ```text
    let user = User(id=1, name="Alice")
    user.getId();  // 1
    ``` 

### 式

#### 演算子

```text
x + y   // add
x - y   // sub
x * y   // mul
x / y   // div
x == y  // equal
x != y  // not equal
x < y   // less than
x > y   // greater than
x <= y  // less than or equal
x >= y  // greater than or equal
x && y  // logical and
x || y  // logical or
!x      // logical not
+x      // unary plus
-x      // unary minus
```

#### If文

```text
let x = if (flag) { 1 } else { 2 }
```

- 式として評価される
- else節は省略できない

#### 関数呼び出し

```text
let x = double(2)

print(text="hello", flag=true)
```

- 引数名を指定して引数を渡すことができる

#### リテラル

```text
// number
integer = 123
float = 123.456

// string
str = "abc" 

// boolean
trueValue = true
falseValue = true
```

#### 構造体の初期化

```text
let user = User(1, "Alice")
```

#### ラムダ式

```text
let callback = function(x:number) { return x * 2 }
```

- 関数名は指定できない
- 引数の型は省略できない
- 戻り値の型は省略可能である


### その他

#### コメント

ブロックコメントと行コメントに対応している。
コメントはプログラムの解析に影響を与えない。

```text
// コード例
let x:number = /* ブロックコメント */ 1
```


#### セミコロンの省略

文の最後のセミコロンは基本的に省略することができる。
ただし、文の区切りが非自明な場合は省略することができない。

<details>
    <summary>正確な定義</summary>

以下の条件の場合、セミコロンを省略できる。
- 文の直後に改行がある
- 文の末尾または直後に `}` がある
- プログラム全体の最後の文である

省略できない例
- 式文が同一行内に連続している: `x=1 y=2` -> `x=1; y=2`

</details>


