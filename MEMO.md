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

* ネイティブ関数の引数が違うときにエラーにならない
* import/define-libraryがうごくようにする
  - prelude.scmの中身を(scheme base)にする
  - include を動くようにする
  - importでの自動ファイル検索を追加
  * デフォルトのインポートを実装(import するまで有効とか？)
  - デフォルトのロードパスを設定
* テストスイートを選定して持ってくる(chibi? gauche? chicken?)
* ifの構文チェックを normalize のほうにもってくる
* vmの初期化でエラーが起きるとdumpができない

R7RS準拠
* 衛生的マクロがちゃんとうごくように rename を実装する
* `(lambda (x) )`がスタック間違いを起こす( `ret` だけで pushがないため)
- charを型に加える
* importした束縛の変更のために、ValueReference(Slot?)的なものを作る
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

# syntactic closure memo

- [explicit\-renaming macroの拡張 \- \.mjtの日記復帰計画](https://mjt.hatenadiary.com/entry/20110302/p1)
- [npca2014年部誌\_アリスプの少女 ハイジニックマクロ\.pdf](file:///C:/Users/harada/Downloads/npca2014%E5%B9%B4%E9%83%A8%E8%AA%8C_%E3%82%A2%E3%83%AA%E3%82%B9%E3%83%97%E3%81%AE%E5%B0%91%E5%A5%B3%20%E3%83%8F%E3%82%A4%E3%82%B8%E3%83%8B%E3%83%83%E3%82%AF%E3%83%9E%E3%82%AF%E3%83%AD.pdf)
- [識別子 \(Gauche ユーザリファレンス\)](https://practical-scheme.net/gauche/man/gauche-refj-draft/Shi-Bie-Zi-.html)

synclo union
    struct {
      sexp env, free_vars, expr, rename;
    } synclo;

SEXP_SYNCLO sexpタイプ

mac-env = マクロ作成時の環境
use-env = マクロ呼び出し時の環境

; 呼び出し階層

*=ここでだけ使われているもの

define-syntax/let-syntax/letrec-syntax
  *%define-syntax/%let-syntax/%letrec-syntax
  *make-transformer
    current-usage-environment
    current-transformer-environment
    current-renamer
    *make-renamer
      close-syntax
        *make-syntactic-closure
      *syntactic-closure-set-rename!


er-macro-transformer
  current-renamer ; == rename
  free-identifier=? ; == compare
    *identifier=?
    current-usage-environment
    current-environment

sc-macro-transformer
  close-syntax
  current-usage-environment
  current-transformer-environment

rsc-macro-transformer
  current-transformer-environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 展開形 (define-syntax id form) => (%define-syntax id (make-transformer form))
(%define-syntax define-syntax
  (lambda (expr use-env mac-env) ...)


; explicit renaming
(define (er-macro-transformer f)
  (lambda (expr)
    (f expr (current-renamer) free-identifier=?)))

; syntactic closure
(define (sc-macro-transformer f)
  (lambda (expr)
    (close-syntax (f expr (current-usage-environment))
      (current-transformer-environment)))))

; reverse syntactic closure
(define (rsc-macro-transformer f)
  (lambda (expr)
    (f expr (current-transformer-environment)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; syncloを作成する
;; sc-macro-transformer以外では、form=symbolのみ?
(close-syntax form env)

;; syncloを作成する
;; ほぼ, close-syntaxでのみ使用
;; freevarは'()で初期化する
;; symbol,pair,synclo以外ならそのまま返す。そうでないなら、syncloを作成して、env,freevar,exprを設定する
(make-syntactic-closure env freevar form) = eval.c:sexp_make_synclo_op()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rename/free-identifer=?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; identifierをmac-envの環境でrenameしたsyncloを返す
; identifier: symbol
; mac-env.renamesに存在するならそれを返す、しないなら、
(rename identifier)

;; renamerを作成する
;; synclo.env = mac-env
;; synclo.free_vars = '()
;; synclo.expr =
;; synclo.rename = ここで作成されたrenamer
(make-renamer mac-env) 
  returns (rename identifier) 

(define (make-renamer mac-env)
  (let ((renames '()))
    (lambda (identifier)
      (let ((cell (assq identifier renames))))
        (if cell
          (cdr cell) ; renamesに存在するならそれを返す
          (let ((id (close-syntax identifier mac-env))) ; 存在しないなら
            (syntactic-closure-set-rename! id rename) ; id.synclo.renameを設定する
            (set! renames (cons (cons identifier id) renames) ; renamesに (identifier . id) を追加する
            id))))))

;; synclo.rename を設定する
(syntactic-closure-set-rename! id rename) = 組み込み


;; current-usage-environmentのx と y が同じかを判定する (current-usage-environmentが#fの場合、current-enviromentが使用される)
;; er-macro-transformer における compare
(free-identifier=? x y)

;; env1のid1 と env2のid2 が同じか判定する
(identifier=? env1 id1 env2 id2) = eval.c:sexp_identifier_eq_op()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; setter/getter
(current-usage-environment use-env)
(current-transformer-environment mac-env)
(current-renamer (make-renamer mac-env))

