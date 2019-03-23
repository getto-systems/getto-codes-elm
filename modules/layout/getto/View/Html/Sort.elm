module GettoCodes.View.Html.Sort exposing
  ( render
  )
import GettoCodes.View.Html as Html
import GettoCodes.View.Icon as Icon

import Getto.Sort as Sort

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias RenderModel msg =
  { sort : Sort.Value
  , msg  : Sort.Value -> msg
  }

render : RenderModel msg -> String -> List (Html msg) -> List (Html msg)
render model column inner =
  let
    {current,next} = model.sort |> Sort.stateOf column
  in
    [ H.a
      ( List.append
        [ "#" |> A.href
        , next |> model.msg |> E.onClick
        ]
        ( case current of
          Nothing -> []
          Just _  -> [ "is-active" |> A.class ]
        )
      )
      ( List.append inner
        [ " " |> H.text
        , Html.icon [] <|
          case current of
            Nothing        -> Icon.fas "sort"
            Just Sort.Up   -> Icon.fas "sort-up"
            Just Sort.Down -> Icon.fas "sort-down"
        ]
      )
    ]
