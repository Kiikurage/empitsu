# 自作言語

特徴：シンプル、手軽に使える、ほどよくルーズ

# 実装状況一覧

- [ ] プリミティブ・組み込み型・標準ライブラリ
  - [x] 数値型(整数・小数の区別なし)
  - [x] 文字列型
  - [x] 真偽値型
  - [ ] 関数
    - [x] 関数宣言・呼び出し
    - [ ] 関数オブジェクト
    - [x] クロージャ
  - [ ] 構造体
  - [ ] 正規表現
  - [ ] 配列
  - [ ] Map
  - [ ] Set
  - [ ] テストフレームワーク
- [ ] 文法
  - [ ] コメント
    - [ ] 行コメント
    - [ ] ブロックコメント
  - [x] if文
  - [x] if式
  - [x] for-in文
  - [ ] match文
    - [ ] 値でのマッチ
    - [ ] 型でのマッチ
    - [ ] 複雑な構造のマッチ
  - [ ] match式
  - [ ] return文
  - [ ] return式
  - [ ] break文
  - [ ] break式
  - [ ] モジュール
  - [ ] 型宣言
    - [ ] インターフェース・トレイトベースの実装
    - [ ] ジェネリクス
    - [ ] 型エイリアス
    - [ ] ユニオン型
  - [ ] オーバーロード
  - [ ] getter/setter
- [ ] 静的型検査
  - [ ] ジェネリクス
  - [ ] アクセス修飾子
  - [ ] mutability
- [ ] GC
- [ ] 周辺ツール
  - [ ] REPL
  - [ ] CI
  - [ ] シンタックスハイライト・エディタプラグイン
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

