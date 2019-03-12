module GettoUpload.App.Upload.ListEdit.Search exposing
  ( Msg
  , init
  , encodeQuery
  , decodeQuery
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.ListEdit.Model as Model
import GettoUpload.App.Upload.ListEdit.Search.View as View
import GettoUpload.App.Upload.ListEdit.Search.Html as Html
import GettoUpload.App.Upload.ListEdit.Info as Info
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present
import Getto.Sort as Sort

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type Msg
  = Info Info.Msg
  | Input (View.Prop String) String
  | Change
  | PageTo String
  | SortBy Sort.Model
  | Request
  | StateChanged (HttpView.Migration View.Response)

signature = "search"

get : Http.Tracker Model.Frame View.Response
get = Http.tracker "get" <|
  \model ->
    let
      m = model |> Frame.app |> .search
    in
      Http.get
        { url     = "uploads" |> Api.url []
        , headers = model |> Api.headers
        , params  = QueryEncode.object
          [ ( "q"
            , [ ( "name",   m.form.name   |> Field.value |> QueryEncode.string )
              , ( "gender", m.form.gender |> Field.value |> QueryEncode.string )
              ] |> QueryEncode.object
            )
          , ( "page", m.page |> QueryEncode.int )
          , ( "sort"
            , case m.sort |> Sort.expose of
              (column,order) ->
                [ ( "column", column |> QueryEncode.string )
                , ( "order",  order  |> QueryEncode.string )
                ] |> QueryEncode.object
            )
          ]
        , response = View.response
        , timeout = 10 * 1000
        }

getTrack   = Http.track   signature get StateChanged
getRequest = Http.request signature get StateChanged


init : Frame.InitModel -> ( Model.Search, Model.Transition Msg )
init model =
  ( { form = View.init signature
    , page = 0
    , sort = "id" |> Sort.by
    , get  = HttpView.empty
    , info = Info.init signature
    }
  , [ getRequestAndPushUrl
    , fill
    ] |> T.batch
  )

encodeQuery : Model.Search -> QueryEncode.Value
encodeQuery model = QueryEncode.object
  [ ( "q",    model.form |> View.encodeForm )
  , ( "page", model.page |> QueryEncode.int )
  , ( "sort"
    , case model.sort |> Sort.expose of
      (column,order) ->
        [ ( "column", column |> QueryEncode.string )
        , ( "order",  order  |> QueryEncode.string )
        ] |> QueryEncode.object
    )
  ]

decodeQuery : List String -> QueryDecode.Value -> Model.Search -> Model.Search
decodeQuery names value model =
  { model
  | form = model.form |> View.decodeForm (names ++ ["q"]) value
  , page = value |> QueryDecode.entryAt (names ++ ["page"]) QueryDecode.int |> Maybe.withDefault model.page
  , sort =
    ( value |> QueryDecode.entryAt (names ++ ["sort","column"]) QueryDecode.string
    , value |> QueryDecode.entryAt (names ++ ["sort","order"])  QueryDecode.string
    ) |> Sort.fromString |> Maybe.withDefault model.sort
  }

encodeStore : Model.Search -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model.Search -> Model.Search
decodeStore value model = model

subscriptions : Model.Search -> Sub Msg
subscriptions model = getTrack

info_ = T.prop .info (\v m -> { m | info = v })

update : Msg -> Model.Search -> ( Model.Search, Model.Transition Msg )
update message model =
  case message of
    Info msg -> model |> T.update info_ (Info.update msg >> T.map Info)

    Input prop value -> ( { model | form = model.form |> Form.set prop value }, T.none )
    Change -> ( model, T.none )

    PageTo page -> ( { model | page = page |> toPage }, getRequestAndPushUrl )
    SortBy sort -> ( { model | sort = sort },           getRequestAndPushUrl )
    Request     -> ( { model | page = 0 },              getRequestAndPushUrl )

    StateChanged mig ->
      ( { model
        | get  = model.get  |> HttpView.update mig
        , info = model.info |>
          if mig |> HttpView.isComplete
            then Info.clear
            else identity
        }
      , Frame.fixedMidashi
      )

toPage : String -> Int
toPage = String.toInt >> Maybe.withDefault 0

fill : Model.Transition Msg
fill = Frame.app >> .search >> .form >> Html.pairs >> Dom.fill

getRequestAndPushUrl : Model.Transition Msg
getRequestAndPushUrl =
  [ getRequest
  , Frame.pushUrl
  ] |> T.batch

contents : Model.Frame -> List (Html Msg)
contents model =
  [ H.section [ A.class "list" ]
    [ H.section [ "search" |> A.class ]
      [ model |> search
      ]
    , H.section [ "data" |> A.class ]
      [ model |> paging
      , model |> table
      , model |> paging
      ]
    ]
  ]

search : Model.Frame -> Html Msg
search model = L.lazy
  (\m -> Html.search
    { view = m.form |> View.view
    , get  = m.get
    , options =
      { gender =
        [ ( "", "select-nothing" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      }
    , msg =
      { request = Request
      , input   = Input
      , change  = Change
      }
    , i18n =
      { field = I18n.field
      , form  = AppI18n.form
      , http  = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .search)

paging : Model.Frame -> Html Msg
paging model = L.lazy
  (\m -> Html.paging
    { page = m.page
    , get  = m.get
    , msg =
      { page = PageTo
      }
    , i18n =
      { paging = AppI18n.paging
      }
    }
  )
  (model |> Frame.app |> .search)

table : Model.Frame -> Html Msg
table model = L.lazy
  (\m -> Html.table
    { get  = m.get
    , sort = m.sort
    , info = m.info |> Info.info
    , msg =
      { sort = SortBy
      , info = Info
      }
    , i18n =
      { field = I18n.field
      , table = AppI18n.table
      , form  = AppI18n.form
      }
    }
  )
  (model |> Frame.app |> .search)

dialogs : Model.Frame -> List (Html Msg)
dialogs model = []
