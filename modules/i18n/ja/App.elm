module GettoUpload.I18n.App exposing
  ( title
  , menu
  , role
  )
import GettoUpload.Extension.Href as Href exposing ( Href )

menu : String -> String
menu = String.toUpper

title : Href -> String
title href =
  case href |> Href.path of
    Href.Internal "index.html" -> "ホーム"

    _ -> href |> Href.toString

role : String -> String
role name =
  case name of
    "admin" -> "管理"

    _ -> name
