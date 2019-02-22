module GettoUpload.View.Html.Table exposing
  ( config
  )

import Getto.Html.Table as Table

import Html as H
import Html.Attributes as A

type alias ConfigModel =
  { emptyMessage : String
  }

config : ConfigModel -> Table.Config msg
config model =
  { attr =
    { table = \data ->
      [ [ "div-auto-size: none"
        , "rows: " ++ (data.thead |> String.fromInt)
        ]
        |> String.join "; "
        |> A.attribute "_fixedhead"
      ]
    , summary = [ "border-top-single" |> A.class ]
    , cell = \border -> List.map A.class <|
      case border of
        ( Table.None, _ ) -> []
        ( Table.Single, Table.Left ) -> ["border-left"]
        ( Table.Double, Table.Left ) -> ["border-left-double"]
        ( Table.Single, Table.Right ) -> ["border-right"]
        ( Table.Double, Table.Right ) -> ["border-right-double"]
    }
  , emptyContent = Table.td []
    [ H.p [ "alert" |> A.class ]
      [ model.emptyMessage |> H.text ]
    ]
  }
