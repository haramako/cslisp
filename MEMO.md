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
- values
- stasticsに関数ごとのコールを追加

* require/module が動くようにする
* 衛生的マクロがちゃんとうごくように rename を実装する
* Func0~ などは、ctxをとらないようにする(restoreRegistersをしなくていいように)
* `(lambda (x) )`がスタック間違いを起こす( `ret` だけで pushがないため)
* 末尾再帰の最適化
* テストスイートを選定して持ってくる(chibi? gauche? chicken?)
* ifの構文チェックを normalize のほうにもってくる
* charを型に加える

* 継続的ベンチマーク
* VMのコードを uint32 にする
* 環境へのアクセスをup, idx でするようにする

* Specialは別の型にする
* 行番号が全部はいるようにする
* C#からの自動バインディングを実装
* let-syntax などがいまはできないのではないか？
* call/cc がスタック/Dump をコピーするのをやめる
* スカウトスレッド（スクリプトエンジン用）
* ちゃんとした多値を実装する
 