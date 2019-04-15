module GettoCodes.App.I18n exposing
  ( title
  , menu
  , role
  , form
  , error
  , paging
  , table
  , box
  , http
  )
import GettoCodes.Html.Input as Input
import GettoCodes.View.Href as Href
import GettoCodes.View.Http as HttpView

menu : String -> String
menu = String.toUpper

title : Href.Path -> String
title path =
  case path of
    Href.Internal "index.html" -> "ホーム"

    Href.Internal "data/upload/list.html"      -> "アップロード"
    Href.Internal "data/upload/list_edit.html" -> "アップロード(編集)"
    Href.Internal "data/upload/new.html"       -> "アップロード登録"
    Href.Internal "data/upload/edit.html"      -> "アップロード情報"

    Href.Internal "system/profile.html" -> "プロフィール"

    Href.Internal p -> p
    Href.Keycloak p -> "keycloak:" ++ p

role : String -> String
role name =
  case name of
    "admin" -> "管理"

    _ -> name

form : String -> String
form name =
  case name of
    "loading"        -> "読み込み中です"
    "select-nothing" -> "すべて"
    "please-select"  -> "選択してください"

    "has-error" -> "保存できない項目があります"

    "select-file" -> "ファイルを選択"

    "new"    -> "新規登録"
    "detail" -> "詳細"

    "overwrite" -> "上書"
    "revert"    -> "キャンセル"

    "edit"   -> "編集"
    "cancel" -> "キャンセル"

    "save"   -> "保存"
    "saving" -> "保存中"

    "delete"   -> "削除"
    "deleting" -> "削除中"

    "upload"    -> "アップロード"
    "uploading" -> "アップロード中"

    "search"    -> "検索"
    "searching" -> "検索中"

    _ -> name

error : String -> String
error name =
  case name of
    "blank"    -> "入力してください"
    "no-file"  -> "ファイルを選択してください"
    "conflict" -> "別な画面で更新されました"

    _ -> name

paging : Input.Paging -> String
paging info =
  [ info.page + 1 |> String.fromInt
  , "/"
  , info.max |> String.fromInt
  , "ページ"
  ] |> String.join " "

table : String -> String
table name =
  case name of
    "empty-data" -> "データがありません"

    _ -> name

box : String -> String
box name =
  case name of
    "register"   -> "登録"
    "info"       -> "基本情報"
    "unregister" -> "削除"

    _ -> name

http : HttpView.Error -> String
http err =
  case err of
    HttpView.BadUrl _             -> "アクセスエラー"
    HttpView.Timeout              -> "タイムアウトエラー"
    HttpView.NetworkError         -> "ネットワークエラー"
    HttpView.BadRequest           -> "リクエストエラー"
    HttpView.Unauthorized         -> "ログインしなおしてください"
    HttpView.Forbidden            -> "権限がありません"
    HttpView.NotFound             -> "存在しませんでした"
    HttpView.Conflict             -> "ほかの場所で変更されました"
    HttpView.UnprocessableEntity  -> "保存できませんでした"
    HttpView.PreconditionRequired -> "情報が送信されませんでした"
    HttpView.BadStatus _          -> "サーバーエラー"
    HttpView.BadResponse _        -> "レスポンスエラー"
