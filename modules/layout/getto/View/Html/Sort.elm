module GettoUpload.View.Html.Sort exposing
  ( render
  )
import GettoUpload.View.Html as Html
import GettoUpload.View.Icon as Icon

import Getto.Sort as Sort

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias RenderModel msg =
  { current : Sort.Model
  , msg     : Sort.Model -> msg
  }

render : RenderModel msg -> String -> List (Html msg) -> List (Html msg)
render model col inner =
  let
    state = col |> Sort.state model.current
  in
    [ H.a
      ( List.append
        [ "#" |> A.href
        , state |> Sort.next |> model.msg |> E.onClick
        ]
        ( case state |> Sort.current of
          Nothing -> []
          Just _  -> [ "is-active" |> A.class ]
        )
      )
      ( List.append inner
        [ " " |> H.text
        , Html.icon [] <|
          case state |> Sort.current of
            Nothing        -> Icon.fas "sort"
            Just Sort.Up   -> Icon.fas "sort-up"
            Just Sort.Down -> Icon.fas "sort-down"
        ]
      )
    ]
