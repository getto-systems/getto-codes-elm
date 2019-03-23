module GettoCodes.I18n.App.Index exposing
  ( title
  )

title : String -> String
title name =
  case name of
    "example" -> "サンプル"

    _ -> name
