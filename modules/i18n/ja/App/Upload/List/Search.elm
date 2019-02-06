module GettoUpload.I18n.App.Upload.List.Search exposing
  ( title
  , entry
  )

title : String -> String
title name =
  case name of
    "select-file" -> "ファイル選択"

    _ -> name

entry : String -> String
entry name =
  case name of
    "order" -> "発注書"

    _ -> name
