module GettoUpload.View.Html.Table exposing
  ( config
  )

import Getto.Html.Table as Table

import Html as H
import Html.Attributes as A

config : (String -> String) -> Table.Config msg
config i18n =
  { attr =
    { table = \data ->
      [ [ "div-auto-size: none"
        , "rows: " ++ (data.thead |> String.fromInt)
        ]
        |> String.join "; "
        |> A.attribute "_fixedhead"
      ]
    , summary = [ "border-top-single" |> A.class ]
    , border = \(left,right) ->
      [ case left of
        Table.None -> []
        Table.Single -> ["border-left"]
        Table.Double -> ["border-left-double"]
      , case right of
        Table.None -> []
        Table.Single -> ["border-right"]
        Table.Double -> ["border-right-double"]
      ]
      |> List.concat |> List.map A.class
    }
  , emptyContent = Table.td []
    [ H.p [ "alert" |> A.class ]
      [ "empty-data" |> i18n |> H.text ]
    ]
  }
