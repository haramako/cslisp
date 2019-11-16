## TODO

- VMコードのダンプなどを追加
- equal?
- exit
- syntax_rules が動くようにする
- minitest.scm を動くようにする
- append
- if分を3項だけとるようにする
- エラーがちゃんと動くようにする
- eval
- call/cc

* require/module が動くようにする
* values
* 衛生的マクロがちゃんとうごくように rename を実装する
* stasticsに関数ごとのコールを追加
* Func0~ などは、ctxをとらないようにする(restoreRegistersをしなくていいように)

* 継続的ベンチマーク
* VMのコードを uint32 にする
* 環境へのアクセスをup, idx でするようにする

* Specialは別の型にする
* 行番号が全部はいるようにする
* C#からの自動バインディングを実装
* let-syntax などがいまはできないのではないか？
* call/cc がスタック/Dump をコピーするのをやめる