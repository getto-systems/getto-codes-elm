module GettoUpload.I18n.App exposing
  ( title
  , menu
  , role
  , form
  )
import GettoUpload.Extension.Href as Href

menu : String -> String
menu = String.toUpper

title : Href.Path -> String
title path =
  case path of
    Href.Internal "index.html" -> "ホーム"

    Href.Internal "upload/list.html" -> "アップロード"
    Href.Internal "upload/new.html"  -> "アップロード登録"

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
    "please-select" -> "選択してください"

    "has-error" -> "保存できない項目があります"

    "select-file" -> "ファイルを選択"

    "save" -> "保存"

    "upload"    -> "アップロード"
    "uploading" -> "アップロード中"

    _ -> name
