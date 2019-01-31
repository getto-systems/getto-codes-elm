module GettoUpload.Layout.Page.Article.Html exposing
  ( documentTitle
  , header
  , footer
  )
import GettoUpload.Extension.View.Icon as Icon
import GettoUpload.Extension.View.Html as Html

import Html as H exposing ( Html )
import Html.Attributes as A


documentTitle : { path : String, company : String, title : String, i18n : String -> String } -> String
documentTitle model =
  (model.path |> model.i18n)
  ++ " | "
  ++ model.company
  ++ " "
  ++ model.title


header : { path : String, i18n : String -> String } -> Html msg
header model =
  H.h1 [] [ model.path |> model.i18n |> H.text ]


footer : { copyright : String } -> Html msg
footer model =
  H.footer []
    [ H.p []
      [ Icon.far "copyright" |> Html.icon []
      , " " |> H.text
      , model.copyright |> H.text
      ]
    ]
