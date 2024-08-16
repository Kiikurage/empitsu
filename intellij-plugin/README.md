# Intellij Plugin

IntelliJ系IDEにEmpitsu言語サポートを追加するプラグイン

## 開発

- `src/main/gen`以下は自動生成されたソースコードである
  - IntelliJ系IDEに[Grammar-Kit](https://plugins.jetbrains.com/plugin/6606-grammar-kit)プラグインを追加
  - `src/grammer/Empitsu.bnf`
    - Parserの定義ファイル
    - Parserの生成
      - IDE上でコンテキストメニューから`Generate Parser Code`を実行
  - `src/grammer/Empitsu.flex`
    - Lexerの定義ファイル
    - Lexerの生成
      - IDE上でコンテキストメニューから`Run JFlex Generator`を実行

- デバッグ実行

  ```
  ./gradlew runIde
  ```
      
- プラグインのビルド

  ```
  ./gradlew buildPlugin
  ```