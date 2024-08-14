# 自作言語

特徴：シンプル、手軽に使える、ほどよくルーズ、富豪プログラミング推奨

TODO:
  - [ ] セミコロンの省略を行末でのみ許可
  - [ ] 文法レベルでのOptionのサポート
      - [ ] built-in type null
      - [ ] TypeExpression::Option(TypeExpression) = "T?" (:= T | null)
      - [ ] TypeExpression::Union(Vec<TypeExpression>) = "T1 | T2 | T3"
  - [ ] 構造体の実行時型検査
  - [ ] 型検査器をVMから切り離す
  - [ ] 静的型検査

# 実装状況一覧

- [ ] プリミティブ・組み込み型・標準ライブラリ
  - [x] 数値型(整数・小数の区別なし)
  - [x] 文字列型
  - [x] 真偽値型
  - [x] 関数
    - [x] 関数宣言・呼び出し
    - [x] 関数オブジェクト
    - [x] クロージャ
  - [x] 参照
  - [x] 構造体
  - [ ] 正規表現
  - [ ] 配列
  - [ ] Error
  - [ ] Map
  - [ ] Set
  - [ ] テストフレームワーク
- [ ] 処理系の強化
  - [ ] リッチなエラー報告
  - [ ] GC
    - [x] 基本的なマーク・スイープの機構
    - [ ] 自動GC実行
  - [ ] バイトコード
- [ ] 文法
  - [x] コメント
    - [x] 行コメント
    - [x] ブロックコメント
  - [x] if文
  - [x] if式
  - [x] for-in文
  - [ ] match文
    - [ ] 値でのマッチ
    - [ ] 型でのマッチ
    - [ ] 複雑な構造のマッチ
  - [ ] match式
  - [x] return文
  - [x] return式
  - [x] break文
  - [x] break式
  - [ ] モジュール
  - [ ] 型宣言
    - [x] 変数
    - [x] 関数の引数
    - [ ] タプル型
    - [ ] 関数の戻り値
    - [ ] インターフェース・トレイト・型クラス
    - [ ] ジェネリクス
    - [ ] エイリアス
    - [ ] ユニオン型
  - [ ] オーバーロード
  - [ ] getter/setter
- [ ] 静的型検査
  - [ ] ジェネリクス
  - [ ] アクセス修飾子
  - [ ] mutability
- [ ] 周辺ツール
  - [x] REPL
  - [ ] CI
  - [ ] シンタックスハイライト・エディタプラグイン
  - [ ] Language Server
  - [ ] Linter

  
# 言語に欲しい機能

- Disposable (TS)using (Python)with
- 宣言的な型システム
    - 型推論
    - ジェネリクス
    - 型クラス(≒インターフェス、トレイト)
- シンプルな文法
    - 関数宣言・メソッド宣言・関数オブジェクトの文法が一貫している
    - なるべく使い慣れた用語を使う
- パターンマッチ
    - メンバのバインド
- 例外処理
    - 型レベルでのサポート: 
        - 関数宣言: function fn() -> T | FileNotFoundError
        - 呼び出し: let t = fn()?;
        - エラーのキャッチ: match fn() { t: T => t, e: FileNotFoundError => ... }
    - 文法レベルでのサポート <- 好き
        - 関数宣言: function fn() -> T (throws FileNotFoundError)
            - throws部分はコンパイラが推論できないか?
        - 呼び出し: let t = fn()
        - エラーのキャッチ: try { fn() } catch (e: FileNotFoundError) { ... }
- モジュールシステム
- オーバーロード
- 演算子オーバーロード
- ユニオン型
- 非同期処理の言語サポート
- ファイルレベルでのモジュール
- 標準ライブラリ・標準コマンドラインでのテストのサポート
- 配列・イテレータ操作系の関数(kotlin!)
- 同名の変数の再宣言
- 暗黙の参照渡し
- DSLをサポートするための仕組み
    - マクロベース?
- アクセス修飾子
- 明示的なmutabilityの管理・デフォルトimmutable
- getter setterのシンタックスシュガー
- for式 return式 if式 throw式 while式 ...
- シンプルなビルドシステム
- alias 
    - エイリアス alias A = B
    - ユニオン型 alias A = B | C
- interface
    - 型制約を作る場合
- リテラル
    - 文字列リテラル
        - 変数展開
    - 数値リテラル
    - 配列リテラル
    - オブジェクトリテラル
    - boolリテラル
    - 正規表現リテラル
- リテラル型
    - リテラルを型として使える
    - リテラル型でのオーバーロード


# いらない機能
- 過度なメモリ最適化
- ライフタイム
    - めんどい
- 継承
    - めんどい
    - バグのもと
- 文法レベルでのモジュール
    - わかりづらい
- インデントベースの文法
    - わかりづらい
    - きもい
- null
    - バグのもと
- アドレス操作・生ポインタ
    - バグのもと
    - めんどい


# どっちともいえない
- マクロ
    - 黒魔術の温床
- リフレクション
    - 黒魔術の温床
- リッチな型演算
    - 黒魔術の温床
- 所有権
- 文法レベルでのリアクティブ性のサポート
- ラベル
    - コードスメル感がある


# If文とIf式

- If文
    - "if" <condition:Expression> <true:Statement> [ "else" <false:Statement> ] 
- If式
    - "if" <condition:Expression> <true:Block|Expression> "else" <false:Block|Expression>
    
# Blockの評価値

Block内の最後の式をBlockの評価値とする

# ループ

```text
for (v in iterator) { ... }
for (v in 0 to 10) { ... }

function f() {
    print(x);
}

{
    let x = 10;
    f(); 
}
```

