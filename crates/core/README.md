# core

Empitsu言語の処理系コア

## TODO

1. 構造体宣言時に構造体にインスタンスメソッドを生やす
2. 構造体宣言時に構造体にスタティックメソッドを生やす
3. すでに宣言されている構造体にインターフェースを実装する

```text
interface ToString {
    fn toString(self): string
}

impl ToString for User {
    fn toString(self): string {
        return self.name
    }
}

fn print(value: ToString) {
    println(value.toString());
}

print(user);
```

```text
}
```
- インターフェース機構
- 演算子オーバーロード


- Clone乱用を見直す
- GCの修正
    - mark時にstructからstruct定義を辿る
    - 参照されなくなった関数やstruct定義のGC
- Nodeにソースコードでの位置情報を埋め込む
- VMのエラーメッセージをリッチにする
- TypeCheckerのエラーメッセージをリッチにする
