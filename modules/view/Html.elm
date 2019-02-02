module GettoUpload.View.Html exposing
  ( icon
  , badge
  )
import GettoUpload.View.Icon as Icon exposing ( Icon )

import Html as H exposing ( Html )
import Html.Attributes as A

icon : List String -> Icon -> Html msg
icon append i =
  case i of
    Icon.Fa fa ->
      let
        class =
          case fa of
            Icon.FaSolid   name -> "fas fa-" ++ name
            Icon.FaRegular name -> "far fa-" ++ name
      in
        H.i [ class :: append |> String.join " " |> A.class ] []

badge : List String -> String -> Html msg
badge append text =
  H.em [ "badge" :: append |> String.join " " |> A.class ]
    [ text |> H.text ]
