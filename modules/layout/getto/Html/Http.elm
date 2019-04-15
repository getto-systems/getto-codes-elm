module GettoCodes.Html.Http exposing
  ( progress
  , error
  )
import GettoCodes.View.Http as HttpView
import GettoCodes.View.Icon as IconView
import GettoCodes.Html.Icon as Icon
import GettoCodes.Html.Content as Content

import Html as H exposing ( Html )
import Html.Attributes as A

progress : Maybe HttpView.Progress -> Html msg
progress data =
  case data |> HttpView.progress of
    Nothing -> "" |> H.text
    Just (direction,percentage) ->
      H.mark [ "progress is-" ++ (percentage |> String.fromInt) |> A.class ]
        [ IconView.fas ("arrow-circle-" ++ (if direction then "right" else "left")) |> Icon.toHtml [] ]

error : (HttpView.Error -> String) -> Maybe HttpView.Error -> Html msg
error i18n res =
  case res of
    Nothing  -> ""  |> H.text
    Just err -> err |> i18n |> Content.badge [ "is-small", "is-danger" ]
