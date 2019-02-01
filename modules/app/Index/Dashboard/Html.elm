module GettoUpload.App.Index.Dashboard.Html exposing
  ( example
  )
import GettoUpload.App.Index.Dashboard.View as View
import GettoUpload.Extension.View.Icon as Icon
import GettoUpload.Extension.View.Html as Html
import GettoUpload.Extension.Href as Href exposing ( Href )

import Html as H exposing ( Html )
import Html.Attributes as A


example : { name : String, data : View.Example, page : Href, i18n : { title : Href.Path -> String, name : String -> String } } -> Html msg
example model =
  H.section []
    [ H.h2 [] [ model.name |> model.i18n.name |> H.text ]
    , H.p []
      [ H.em []
        [ model.data.current |> H.text
        , H.small [] [ H.small [] [ "million" |> H.text ] ]
        ]
      , " " |> H.text
      , H.em [] [ "/" |> H.text ]
      , " " |> H.text
      , H.em []
        [ H.small [] [ model.data.target |> H.text ]
        , H.small [] [ H.small [] [ "million" |> H.text ] ]
        ]
      ]
    , H.meter [ model.data.current |> A.value, model.data.max |> A.max ]
      [ model.data.percent ++ "%" |> H.text ]
    , H.footer []
      [ H.a [ model.page |> Href.toString |> A.href ]
        [ Icon.fas "user" |> Html.icon []
        , " " |> H.text
        , model.page |> Href.path |> model.i18n.title |> H.text
        ]
      ]
    ]
