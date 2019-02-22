module Getto.Html.Table exposing
  ( Config
  , BorderStyle(..)
  , render
  , column
  , group
  , union
  , parts
  , rows
  , none
  , single
  , double
  , empty
  , th
  , td
  )
import Getto.Html.Table.Struct as Struct

import Html as H exposing ( Html )
import Html.Attributes as A

type BorderStyle
  = None
  | Single
  | Double

type alias HtmlCell msg =
  { tag  : List (H.Attribute msg) -> List (Html msg) -> Html msg
  , attr : List (H.Attribute msg)
  , body : List (Html msg)
  }

column = Struct.column
group  = Struct.group
union  = Struct.union
parts  = Struct.parts
rows   = Struct.rows

none   = Struct.None
single = Struct.Single
double = Struct.Double

type alias Config msg =
  { attr :
    { table   : { thead : Int } -> List (H.Attribute msg)
    , summary : List (H.Attribute msg)
    , border  : BorderAttribute msg
    }
  , emptyContent : Struct.Cell (HtmlCell msg)
  }
type alias BorderAttribute msg = ( BorderStyle, BorderStyle ) -> List (H.Attribute msg)

render : Config msg -> List (Struct.Column row (HtmlCell msg)) -> List row -> Html msg
render config columns list =
  let
    data = list |> Struct.render columns
      { emptyContent = config.emptyContent
      , render       = cell config.attr.border
      }

    thead = List.concat
      [ data.header  |> List.map (H.tr [])
      , data.summary |> List.map (H.tr config.attr.summary)
      ]

    tbody = data.content |> List.map (H.tr [])
  in
    H.table ( { thead = thead |> List.length } |> config.attr.table )
      [ H.thead [] thead
      , H.tbody [] tbody
      ]

cell : BorderAttribute msg -> Struct.Render (HtmlCell msg) (Html msg)
cell attr border info data =
  let
    base = List.concat
      [ [ info.rowspan |> A.rowspan
        , info.colspan |> A.colspan
        ]
      , border |> Tuple.mapBoth mapBorderStyle mapBorderStyle |> attr
      ]
  in
    case data of
      Struct.Empty -> H.td base []
      Struct.Cell html -> html.tag (base ++ html.attr) html.body

mapBorderStyle : Struct.BorderStyle -> BorderStyle
mapBorderStyle style =
  case style of
    Struct.None -> None
    Struct.Single -> Single
    Struct.Double -> Double


empty : Struct.Cell (HtmlCell msg)
empty = Struct.Empty

th : List (H.Attribute msg) -> List (Html msg) -> Struct.Cell (HtmlCell msg)
th attr body = Struct.Cell
  { tag  = H.th
  , attr = attr
  , body = body
  }

td : List (H.Attribute msg) -> List (Html msg) -> Struct.Cell (HtmlCell msg)
td attr body = Struct.Cell
  { tag  = H.td
  , attr = attr
  , body = body
  }
