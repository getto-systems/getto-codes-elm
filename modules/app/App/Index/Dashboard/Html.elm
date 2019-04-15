module GettoCodes.App.Index.Dashboard.Html exposing
  ( example
  )
import GettoCodes.App.Index.Dashboard.View as View
import GettoCodes.View.Href as Href exposing ( Href )
import GettoCodes.View.Icon as IconView
import GettoCodes.Html.Icon as Icon

import Html as H exposing ( Html )
import Html.Attributes as A


type alias ExampleModel =
  { data : View.Example
  , page : Href
  , i18n :
    { title : Href.Path -> String
    , name  : String -> String
    }
  }

example : ExampleModel -> Html msg
example model =
  H.section []
    [ H.h2 [] [ "example" |> model.i18n.name |> H.text ]
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
        [ IconView.fas "user" |> Icon.toHtml []
        , " " |> H.text
        , model.page |> Href.path |> model.i18n.title |> H.text
        ]
      ]
    ]
