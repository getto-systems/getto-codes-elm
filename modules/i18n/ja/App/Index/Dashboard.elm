module GettoUpload.I18n.App.Index.Dashboard exposing
  ( title
  )

title : String -> String
title name =
  case name of
    "example" -> "サンプル"

    _ -> name
