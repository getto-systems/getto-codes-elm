module GettoCodes.Html.Content exposing
  ( badge
  )

import Html as H exposing ( Html )
import Html.Attributes as A

badge : List String -> String -> Html msg
badge append text =
  H.em [ "badge" :: append |> String.join " " |> A.class ]
    [ text |> H.text ]
