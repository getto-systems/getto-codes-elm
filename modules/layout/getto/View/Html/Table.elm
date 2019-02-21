module GettoUpload.View.Html.Table exposing
  ( attr
  , emptyData
  )

import Getto.Html.Table as Table

import Html as H
import Html.Attributes as A

attr : Table.Attribute msg
attr =
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

emptyData : String -> Table.Cell msg
emptyData text = Table.td []
  [ H.p [ "alert" |> A.class ]
    [ text |> H.text ]
  ]
