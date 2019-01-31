module GettoUpload.I18n.App exposing
  ( title
  , menu
  , role
  )

menu : String -> String
menu = String.toUpper

title : String -> String
title path =
  case path of
    "index.html" -> "ホーム"

    _ -> path

role : String -> String
role name =
  case name of
    "admin" -> "管理"

    _ -> name
