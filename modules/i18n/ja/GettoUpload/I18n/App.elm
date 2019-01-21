module GettoUpload.I18n.App exposing
  ( title
  )

title : String -> String
title path =
  case path of
    "index.html" -> "ホーム"

    _ -> path
