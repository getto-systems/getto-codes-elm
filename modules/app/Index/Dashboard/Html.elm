module GettoUpload.App.Index.Dashboard.Html exposing
  ( example
  )
import GettoUpload.Extension.View.Icon as Icon
import GettoUpload.Extension.View.Html as Html
import GettoUpload.Extension.Href as Href exposing ( Href )

import Html as H exposing ( Html )
import Html.Attributes as A


example : { name : String, current : Int, target : Int, page : Href, i18n : { title : String -> String, name : String -> String } } -> Html msg
example model =
  let
    current = model.current |> String.fromInt
    target  = model.target  |> String.fromInt
    all     = (model.target * 3 |> toFloat) / 2
    max     = all |> String.fromFloat
    percent = (model.current * 100 |> toFloat) / all |> floor |> String.fromInt
  in
    H.section []
      [ H.h2 [] [ model.name |> model.i18n.name |> H.text ]
      , H.p []
        [ H.em []
          [ current |> H.text
          , H.small [] [ H.small [] [ "million" |> H.text ] ]
          ]
        , " " |> H.text
        , H.em [] [ "/" |> H.text ]
        , " " |> H.text
        , H.em []
          [ H.small [] [ target |> H.text ]
          , H.small [] [ H.small [] [ "million" |> H.text ] ]
          ]
        ]
      , H.meter [ current |> A.value, max |> A.max ] [ percent ++ "%" |> H.text ]
      , H.footer []
        [ H.a [ model.page |> Href.toString |> A.href ]
          [ Icon.fas "user" |> Html.icon []
          , " " |> H.text
          , model.page |> Href.path |> model.i18n.title |> H.text
          ]
        ]
      ]
