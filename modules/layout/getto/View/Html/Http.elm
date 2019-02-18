module GettoUpload.View.Html.Http exposing
  ( progress
  , error
  )
import GettoUpload.View.Http as HttpView
import GettoUpload.View.Icon as Icon
import GettoUpload.View.Html as Html

import Html as H exposing ( Html )
import Html.Attributes as A

progress : Maybe HttpView.Progress -> Html msg
progress data =
  case data |> HttpView.progress of
    Nothing -> "" |> H.text
    Just (direction,percentage) ->
      H.mark [ "progress is-" ++ (percentage |> String.fromInt) |> A.class ]
        [ Icon.fas ("arrow-circle-" ++ (if direction then "right" else "left")) |> Html.icon [] ]

error : (HttpView.Error -> String) -> Maybe HttpView.Error -> Html msg
error i18n res =
  case res of
    Nothing  -> ""  |> H.text
    Just err -> err |> i18n |> Html.badge [ "is-small", "is-danger" ]
