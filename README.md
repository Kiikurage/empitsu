# 名前: Enpitsu(仮)

自作プログラミング言語の名前
特徴：シンプル、手軽に使える


# TODO
- break
- function declaration
- return
- 構造体
- 組み込み型
  - 配列

# プログラミング言語

- 言語本体
- リッチな標準ライブラリ
- パッケージマネージャ
- ドキュメント
- コミュニティ
- Linter


# 言語に欲しい機能

- 波括弧{ }ベースのスコープ
- GC
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
```

