module GettoCodes.Layout.Page.Article.Html exposing
  ( documentTitle
  , header
  , footer
  )
import GettoCodes.View.Icon as IconView
import GettoCodes.Html.Icon as Icon
import GettoCodes.Extension.Href as Href

import Html as H exposing ( Html )


documentTitle : { path : Href.Path, company : String, title : String, i18n : Href.Path -> String } -> String
documentTitle model =
  (model.path |> model.i18n)
  ++ " | "
  ++ model.company
  ++ " "
  ++ model.title


header : { path : Href.Path, i18n : Href.Path -> String } -> Html msg
header model =
  H.h1 [] [ model.path |> model.i18n |> H.text ]


footer : { copyright : String } -> Html msg
footer model =
  H.footer []
    [ H.p []
      [ IconView.far "copyright" |> Icon.toHtml []
      , " " |> H.text
      , model.copyright |> H.text
      ]
    ]
