module GettoUpload.I18n.App exposing
  ( title
  , menu
  )

menu : String -> String
menu = String.toUpper

title : String -> String
title path =
  case path of
    "index.html" -> "ホーム"

    _ -> path
