module Getto.Html.Table exposing
  ( BorderStyle(..)
  , Position(..)
  , Attribute
  , Cell
  , table
  , column
  , group
  , union
  , parts
  , rows
  , th
  , td
  , empty
  )

import Html as H exposing ( Html )
import Html.Attributes as A

type Column row msg
  = Column
    { header  : Header msg
    , summary : Summary msg
    , content : List row -> Content msg
    }
  | Group
    { header    : Header msg
    , summaries : List (Summary msg)
    , contents  : List row -> List (Content msg)
    }
  | Union
    { header  : Header msg
    , summary : Summary msg
    , content : List row -> Content msg
    }
  | Parts
    { headers   : List (Header msg)
    , summaries : List (Summary msg)
    , contents  : List row -> List (Content msg)
    }
  | Rows
    { headers   : List (Header msg)
    , summaries : List (Summary msg)
    , content   : List row -> Content msg
    }

type Header msg
  = Header      { border : Border, cell : Cell msg }
  | GroupHeader { border : Border, cell : Cell msg, children : List (Header msg) }
  | UnionHeader { border : Border, cell : Cell msg, colspan : Int }

type Summary msg
  = Summary { border : Border, cell : Cell msg }

type Content msg
  = Content      { border : Border, cells        : List (Cell msg) }
  | UnionContent { border : Border, cellLists    : List (List (Cell msg)),    colspan : Int }
  | RowsContent  {                  contentLists : List (List (Content msg)), colspan : Int }

type Build msg
  = Build      { border : Border, cell       : Cell msg,                colspan : Int }
  | UnionBuild { border : Border, cells      : List (Cell msg),         colspan : Int }
  | RowsBuild  {                  buildLists : List (List (Build msg)), colspan : Int }

type alias Border = ( BorderStyle, BorderStyle )

type BorderStyle
  = None
  | Single
  | Double

type Position
  = Left
  | Right

type Cell msg
  = Empty
  | Th (List (H.Attribute msg)) (List (Html msg))
  | Td (List (H.Attribute msg)) (List (Html msg))

type alias Attribute msg =
  { table   : { thead : Int } -> List (H.Attribute msg)
  , summary : List (H.Attribute msg)
  , cell    : BorderAttribute msg
  }
type alias BorderAttribute msg = ( BorderStyle, Position ) -> List (H.Attribute msg)

table : Attribute msg -> Cell msg -> List (Column row msg) -> List row -> Html msg
table attr emptyData columns list =
  let
    struct = list |> build emptyData columns

    thead = List.concat
      [ struct.header  |> toHeaderRows attr.cell |> List.map (H.tr [])
      , struct.summary |> toSummaryRows attr.summary attr.cell
      ]

    -- TODO この tr に行から生成した attr を指定したいのだが・・・
    tbody = struct.content |> toContentRows attr.cell |> List.map (H.tr [])
  in
    H.table ( { thead = thead |> List.length } |> attr.table )
      [ H.thead [] thead
      , H.tbody [] tbody
      ]

type alias Struct msg =
  { header  : HeaderStruct msg
  , summary : SummaryStruct msg
  , content : ContentStruct msg
  }
type alias HeaderStruct msg =
  { headers : List (Header msg)
  , rowspan : Int
  }
type alias SummaryStruct msg = Maybe (List (Summary msg))
type alias ContentStruct msg = List (List (Build msg))

build : Cell msg -> List (Column row msg) -> List row -> Struct msg
build emptyData columns list =
  let
    struct = columns |> List.foldl
      (\col acc ->
        case col of
          Column data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ [ data.summary ]
            , content = acc.content ++ [ list |> data.content ]
            }
          Group data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ ( list |> data.contents )
            }
          Union data ->
            { acc
            | header  = acc.header  ++ [ data.header ]
            , summary = acc.summary ++ [ data.summary ]
            , content = acc.content ++ [ list |> data.content ]
            }
          Parts data ->
            { acc
            | header  = acc.header  ++ ( data.headers )
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ ( list |> data.contents )
            }
          Rows data ->
            { acc
            | header  = acc.header  ++ ( data.headers )
            , summary = acc.summary ++ ( data.summaries )
            , content = acc.content ++ [ list |> data.content ]
            }
      )
      { header  = []
      , summary = []
      , content = []
      }
  in
    { header  = struct.header  |> buildHeader
    , summary = struct.summary |> buildSummary
    , content = struct.content |> buildContent |> transpose |> withAlert emptyData struct.summary
    }

buildHeader : List (Header msg) -> HeaderStruct msg
buildHeader headers =
  let
    depth header =
      case header of
        GroupHeader data -> 1 + (data.children |> maxDepth depth)
        _ -> 1
  in
    { headers = headers
    , rowspan = headers |> maxDepth depth
    }

buildSummary : List (Summary msg) -> SummaryStruct msg
buildSummary summaries =
  if summaries |> List.all (\(Summary data) -> data.cell == Empty)
    then Nothing
    else Just summaries

buildContent : List (Content msg) -> List (List (Build msg))
buildContent = List.map <|
  \content ->
    case content of
      Content data ->
        data.cells |> List.map
          (\cellValue -> Build
            { border  = data.border
            , cell    = cellValue
            , colspan = 1
            }
          )
      UnionContent data ->
        data.cellLists |> List.map
          (\cells -> UnionBuild
            { border  = data.border
            , cells   = cells
            , colspan = data.colspan
            }
          )
      RowsContent data ->
        data.contentLists |> List.map
          (\contents -> RowsBuild
            { buildLists = contents |> buildContent
            , colspan    = data.colspan
            }
          )

transpose : List (List a) -> List (List a)
transpose listOfLists =
  let
    rowsLength =
      case listOfLists of
        [] -> 0
        head :: _ -> head |> List.length
  in
    List.foldr (List.map2 (::)) (List.repeat rowsLength []) listOfLists

withAlert : Cell msg -> List (Summary msg) -> List (List (Build msg)) -> List (List (Build msg))
withAlert emptyData summaries builds =
  if builds |> List.isEmpty |> not
    then builds
    else
      let
        border =
          ( case summaries |> List.head of
            Just (Summary data) -> data.border |> Tuple.first
            Nothing -> None
          , case summaries |> List.reverse |> List.head of
            Just (Summary data) -> data.border |> Tuple.second
            Nothing -> None
          )
      in
        [ [ Build
            { border  = border
            , cell    = emptyData
            , colspan = summaries |> List.length
            }
          ]
        ]


toHeaderRows : BorderAttribute msg -> HeaderStruct msg -> List (List (Html msg))
toHeaderRows borderAttr struct =
  let
    width header =
      case header of
        UnionHeader data -> data.colspan
        GroupHeader data -> data.children |> List.map width |> List.sum
        _ -> 1

    fullWidth = List.map width >> List.sum
  in
    struct.headers
    |> List.foldl
      (\cellValue -> fill <|
        case cellValue of
          Header data ->
            [ data.cell |> cell borderAttr data.border
              { colspan = 1
              , rowspan = struct.rowspan
              }
            ] :: []
          UnionHeader data ->
            if data.colspan == 0
              then []
              else
                [ data.cell |> cell borderAttr data.border
                  { colspan = data.colspan
                  , rowspan = struct.rowspan
                  }
                ] :: []
          GroupHeader data ->
            [ data.cell |> cell borderAttr data.border
              { colspan = data.children |> fullWidth
              , rowspan = 1
              }
            ] :: ( { headers = data.children, rowspan = struct.rowspan - 1 } |> toHeaderRows borderAttr )
      )
      []

toSummaryRows : List (H.Attribute msg) -> BorderAttribute msg -> SummaryStruct msg -> List (Html msg)
toSummaryRows attr borderAttr =
  Maybe.map
    (\summaries ->
      [ summaries
        |> List.map
          (\(Summary data) ->
            data.cell |> cell borderAttr data.border
              { colspan = 1
              , rowspan = 1
              }
          )
        |> H.tr attr
      ]
    )
  >> Maybe.withDefault []

toContentRows : BorderAttribute msg -> ContentStruct msg -> List (List (Html msg))
toContentRows borderAttr = List.concatMap <|
  \row ->
    let
      depth content =
        case content of
          RowsBuild data -> data.buildLists |> List.map (maxDepth depth) |> List.sum
          _ -> 1

      fullDepth = List.map (maxDepth depth) >> List.sum

      rowspan = row |> maxDepth depth
    in
      row |> List.foldl
        (\col -> fill <|
          case col of
            Build data ->
              [ [ data.cell |> cell borderAttr data.border
                  { colspan = data.colspan
                  , rowspan = rowspan
                  }
                ]
              ]
            UnionBuild data ->
              [ ( data.cells ++
                  ( empty |> List.repeat ( data.colspan - (data.cells |> List.length)) )
                )
                |> List.map
                  (cell borderAttr data.border
                    { colspan = 1
                    , rowspan = rowspan
                    }
                  )
              ]
            RowsBuild data ->
              let
                paddingLength = rowspan - (data.buildLists |> fullDepth)
              in
                ( data.buildLists |> toContentRows borderAttr ) ++
                ( if paddingLength > 0
                    then
                      [ [ empty |> cell borderAttr (data.buildLists |> buildBorder)
                          { colspan = data.colspan
                          , rowspan = paddingLength
                          }
                        ]
                      ]
                    else []
                )
        )
        []

buildBorder : ContentStruct msg -> Border
buildBorder builds =
  ( case builds |> List.head of
    Just (Build      data :: _) -> data.border |> Tuple.first
    Just (UnionBuild data :: _) -> data.border |> Tuple.first
    Just (RowsBuild  data :: _) -> data.buildLists |> buildBorder |> Tuple.first
    _ -> None

  , case builds |> List.reverse |> List.head of
    Just (Build      data :: _) -> data.border |> Tuple.second
    Just (UnionBuild data :: _) -> data.border |> Tuple.second
    Just (RowsBuild  data :: _) -> data.buildLists |> buildBorder |> Tuple.second
    _ -> None
  )

fill : List (List a) -> List (List a) -> List (List a)
fill list acc =
  let
    rowLength = list |> List.length
    accLength = acc  |> List.length

    length =
      if rowLength > accLength
        then rowLength
        else accLength

    expand targetLength target =
      target ++
      ( [] |> List.repeat (length - targetLength) )
  in
    List.map2 List.append
      (acc  |> expand accLength)
      (list |> expand rowLength)


maxDepth : (row -> Int) -> List row -> Int
maxDepth depth =
  List.map depth
  >> List.maximum
  >> Maybe.withDefault 1


type alias CellInfo =
  { colspan : Int
  , rowspan : Int
  }

cell : BorderAttribute msg -> Border -> CellInfo -> Cell msg -> Html msg
cell borderAttr (left,right) info data =
  let
    base = List.concat
      [ [ info.rowspan |> A.rowspan
        , info.colspan |> A.colspan
        ]
      , ( left,  Left )  |> borderAttr
      , ( right, Right ) |> borderAttr
      ]
  in
    case data of
      Empty        -> H.td base []
      Th attr body -> H.th (base ++ attr) body
      Td attr body -> H.td (base ++ attr) body


type alias ColumnModel row msg =
  { header  : Cell msg
  , summary : Cell msg
  , content : row -> Cell msg
  }

column : Border -> ColumnModel row msg -> Column row msg
column border model = Column
  { header  = Header  { border = border, cell = model.header }
  , summary = Summary { border = border, cell = model.summary }
  , content = \list -> Content { border = border, cells = list |> List.map model.content }
  }

type alias GroupModel msg =
  { header : Cell msg
  }

group : GroupModel msg -> List (Column row msg) -> Column row msg
group model columns =
  let
    summary f data =
      case data of
        Column d -> Just d.summary
        Group  d -> d.summaries |> f |> List.head
        Union  d -> Just d.summary
        Parts  d -> d.summaries |> f |> List.head
        Rows   d -> d.summaries |> f |> List.head

    border =
      ( case columns |> List.head |> Maybe.andThen (summary identity) of
        Just (Summary data) -> data.border |> Tuple.first
        Nothing -> None
      , case columns |> List.reverse |> List.head |> Maybe.andThen (summary List.reverse) of
        Just (Summary data) -> data.border |> Tuple.second
        Nothing -> None
      )
  in
    Group
      { header = GroupHeader
        { border   = border
        , cell     = model.header
        , children = columns |> columnHeaders
        }
      , summaries = columns |> columnSummaries
      , contents  = \list -> columns |> columnContents list
      }

type alias UnionModel row data msg =
  { header  : Cell msg
  , summary : Cell msg
  , colspan : Int
  , data    : row -> List data
  , content : data -> Cell msg
  }

union : Border -> UnionModel row data msg -> Column row msg
union border model = Union
  { header  = UnionHeader { border = border, cell = model.header, colspan = model.colspan }
  , summary = Summary     { border = border, cell = model.summary }
  , content = \list -> UnionContent
    { border    = border
    , cellLists = list |> List.map ( model.data >> List.map model.content )
    , colspan   = model.colspan
    }
  }

parts : List data -> (data -> List (Column row msg)) -> Column row msg
parts data f =
  let
    columns = data |> List.concatMap f
  in
    Parts
      { headers   = columns |> columnHeaders
      , summaries = columns |> columnSummaries
      , contents  = \list -> columns |> columnContents list
      }

rows : (row -> List data) -> List (Column data msg) -> Column row msg
rows data columns =
  let
    summaries = columns |> columnSummaries
  in
    Rows
      { headers   = columns |> columnHeaders
      , summaries = summaries
      , content   = \list -> RowsContent
        { contentLists = list |> List.map (\row -> columns |> columnContents (row |> data))
        , colspan      = summaries |> List.length
        }
      }

columnHeaders : List (Column row msg) -> List (Header msg)
columnHeaders = List.concatMap <|
  \col ->
    case col of
      Column data -> [ data.header ]
      Group  data -> [ data.header ]
      Union  data -> [ data.header ]
      Parts  data -> ( data.headers )
      Rows   data -> ( data.headers )

columnSummaries : List (Column row msg) -> List (Summary msg)
columnSummaries = List.concatMap <|
  \col ->
    case col of
      Column data -> [ data.summary ]
      Group  data -> ( data.summaries )
      Union  data -> [ data.summary ]
      Parts  data -> ( data.summaries )
      Rows   data -> ( data.summaries )

columnContents : List row -> List (Column row msg) -> List (Content msg)
columnContents list = List.concatMap <|
  \col ->
    case col of
      Column data -> [ list |> data.content ]
      Group  data -> ( list |> data.contents )
      Union  data -> [ list |> data.content ]
      Parts  data -> ( list |> data.contents )
      Rows   data -> [ list |> data.content ]


empty : Cell msg
empty = Empty

th : List (H.Attribute msg) -> List (Html msg) -> Cell msg
th = Th

td : List (H.Attribute msg) -> List (Html msg) -> Cell msg
td = Td
