module GettoUpload.I18n.App exposing
  ( title
  , menu
  , role
  , form
  , paging
  , table
  )
import GettoUpload.View.Html.Input as Input
import GettoUpload.Extension.Href as Href

menu : String -> String
menu = String.toUpper

title : Href.Path -> String
title path =
  case path of
    Href.Internal "index.html" -> "ホーム"

    Href.Internal "upload/list.html" -> "アップロード"
    Href.Internal "upload/new.html"  -> "アップロード登録"
    Href.Internal "upload/edit.html" -> "アップロード情報"

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

    "upload"    -> "アップロード"
    "uploading" -> "アップロード中"

    "search"    -> "検索"
    "searching" -> "検索中"

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
