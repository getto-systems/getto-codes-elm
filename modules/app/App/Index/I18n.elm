module GettoCodes.App.Index.I18n exposing
  ( title
  )

title : String -> String
title name =
  case name of
    "example" -> "サンプル"

    _ -> name
