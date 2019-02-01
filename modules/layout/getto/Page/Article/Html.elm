module GettoUpload.Layout.Page.Article.Html exposing
  ( documentTitle
  , header
  , footer
  )
import GettoUpload.Extension.View.Icon as Icon
import GettoUpload.Extension.View.Html as Html
import GettoUpload.Extension.Href as Href exposing ( Href )

import Html as H exposing ( Html )
import Html.Attributes as A


documentTitle : { href : Href, company : String, title : String, i18n : Href -> String } -> String
documentTitle model =
  (model.href |> model.i18n)
  ++ " | "
  ++ model.company
  ++ " "
  ++ model.title


header : { href : Href, i18n : Href -> String } -> Html msg
header model =
  H.h1 [] [ model.href |> model.i18n |> H.text ]


footer : { copyright : String } -> Html msg
footer model =
  H.footer []
    [ H.p []
      [ Icon.far "copyright" |> Html.icon []
      , " " |> H.text
      , model.copyright |> H.text
      ]
    ]
