# core

Empitsu言語の処理系コア

## TODO

- 演算子オーバーロード


- Clone乱用を見直す
- GCの修正
    - mark時にstructからstruct定義を辿る
    - 参照されなくなった関数やstruct定義のGC
- Nodeにソースコードでの位置情報を埋め込む
- VMのエラーメッセージをリッチにする
- TypeCheckerのエラーメッセージをリッチにする
