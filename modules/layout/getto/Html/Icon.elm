module GettoCodes.Html.Icon exposing
  ( toHtml
  , spinner
  )
import GettoCodes.View.Icon as IconView exposing ( Icon )

import Html as H exposing ( Html )
import Html.Attributes as A

toHtml : List String -> Icon -> Html msg
toHtml append i =
  case i of
    IconView.Fa fa ->
      let
        class =
          case fa of
            IconView.FaSolid   name -> "fas fa-" ++ name
            IconView.FaRegular name -> "far fa-" ++ name
      in
        H.i [ class :: append |> String.join " " |> A.class ] []

spinner : Html msg
spinner = IconView.spinner |> toHtml ["fa-pulse"]
