

# Google で OAuth

https://console.developers.google.com/project
でプロジェクトを作る。

Contacts API, Google+ API を有効にする。

クライアント ID を作成

* アプリケーションの種類
  ウェブ アプリケーション
* 承認済みの JavaScript 生成元
  http://localhost:1959
* 承認済みのリダイレクト URI
  http://localhost:1959/oauth2callback

google-oauth.lisp に クライアント ID と クライアント シークレット を google-oauth.example.lisp を参考に書く。
