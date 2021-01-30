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
- require/module が動くようにする

* import/define-libraryがうごくようにする
  - prelude.scmの中身を(scheme base)にする
  * include を動くようにする
  * importでの自動ファイル検索を追加
  * デフォルトのインポートを実装(import するまで有効とか？)
  * デフォルトのロードパスを設定
* テストスイートを選定して持ってくる(chibi? gauche? chicken?)
* ifの構文チェックを normalize のほうにもってくる

R7RS準拠
* 衛生的マクロがちゃんとうごくように rename を実装する
* `(lambda (x) )`がスタック間違いを起こす( `ret` だけで pushがないため)
* charを型に加える
* importした束縛の変更のために、ValueReference的なものを作る
* let-syntax などがいまはできないのではないか？
* 行番号が全部はいるようにする
* ちゃんとした多値を実装する

組み込み用途
* C#からの自動バインディングを実装
* スカウトスレッド（スクリプトエンジン用）

パフォーマンス
* 継続的ベンチマーク
* 末尾再帰の最適化
* VMのコードを uint32 にする
* 環境へのアクセスをup, idx でするようにする
* Specialは別の型にする
* call/cc がスタック/Dump をコピーするのをやめる
* Func0~ などは、ctxをとらないようにする(restoreRegistersをしなくていいように)
 

## R7RSメモ

http://milkpot.sakura.ne.jp/scheme/r7rs.pdf

ベクタ
バイトベクタ 
レコード 
ポート 
define-library
import
raise
file-error? read-error?
垂直線 (|) で囲ったゼロ個以上の文字の並びで識別子を表すこともできます。
#!fold-case #!no-fold-case
#( これはベクタ定数の始まりです
#u8( これはバイトベクタ定数
#e #i #b #o #d #x これらは数値を表すために使われます
#⟨n⟩= #⟨n⟩# これらは他のリテラルデータの参照やラベル付け
let、let*、letrec、letrec*、let-values、let*-values および do
オブジェクトも以下の述語を 2 つ以上満たすことはありません。 boolean? bytevector? char? eof-object? null? number? pair? port? procedure? string? symbol? vector? define-record-type?
条件判定では#f 以外のすべての値が真とみなされます。
3.3. 外部表現
場所を指すすべてのオブジェクトは可変または不変のいずれかです。
case-lambda
末尾文脈 if cond case and or when unless let** begin do
この報告書で定義されている一部の手続きも末尾呼び出しを行うことが要求されます。apply の第 1 引数、call-with-current-continuation の第 1 引数およびcall-with-values の第 2 引数は末尾呼び出しで呼び出されなければなりません。
同様に eval の第 1 引数は eval 内の末尾位置で呼び出されたかように評価されなければなりません。
最後の変数の前にスペースで区切られたピリオドがある場合、その手続きは n 個以上の引数を取ります。ただし n はピリオドの前の仮引数の数です (最低ひとつ以上なければエラーです)。
variable⟩ が ⟨formals⟩ に 2 回以上現れる場合はエラーです。
if ⟨alternate⟩ が指定されていない場合、式の結果は規定されていません。
(include, (include-ci 処理系固有のアルゴリズムを適用して対応するファイルを検索...


page14まで読了

ずブーリアン値を返す手続きの名前は最後の文字が ? です。そういった手続きは述語
すでに割り当て済みの場所 (3.4 節を参照) に値を代入する手続きの名前は最後の文字が !
すべての Scheme 処理系は以下の拡張識別子文字をサポートしなければなりません。! $ % & * + - . / : < = > ? @ ^ _ ~