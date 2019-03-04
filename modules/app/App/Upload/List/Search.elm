module GettoUpload.App.Upload.List.Search exposing
  ( Model
  , Msg
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
import GettoUpload.App.Upload.List.Search.View as View
import GettoUpload.App.Upload.List.Search.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present
import Getto.Sort as Sort

import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | search : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form   : View.Form
  , page   : Int
  , sort   : Sort.Model
  , search : HttpView.Model View.Response
  }

type Msg
  = FieldInput  (View.Prop String) String
  | FieldToggle (View.Prop (Set String)) String
  | FieldChange
  | PageTo String
  | SortBy Sort.Model
  | SearchRequest
  | SearchStateChanged (HttpView.Migration View.Response)

signature = "search"

get : Http.Tracker (FrameModel a) View.Response
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
            , [ ( "name",           m.form.name          |> Field.value |> QueryEncode.string )
              , ( "age_gteq",       m.form.age_gteq      |> Field.value |> QueryEncode.string )
              , ( "age_lteq",       m.form.age_lteq      |> Field.value |> QueryEncode.string )
              , ( "email",          m.form.email         |> Field.value |> QueryEncode.string )
              , ( "tel",            m.form.tel           |> Field.value |> QueryEncode.string )
              , ( "birthday_gteq",  m.form.birthday_gteq |> Field.value |> QueryEncode.string )
              , ( "birthday_lteq",  m.form.birthday_lteq |> Field.value |> QueryEncode.string )
              , ( "start_at_gteq",  m.form.start_at_gteq |> Field.value |> QueryEncode.string )
              , ( "start_at_lteq",  m.form.start_at_lteq |> Field.value |> QueryEncode.string )
              , ( "gender",         m.form.gender        |> Field.value |> QueryEncode.string )
              , ( "roles",          m.form.roles         |> Field.value |> QueryEncode.set QueryEncode.string )
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


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form = View.init signature
    , page = 0
    , sort = "id" |> Sort.by
    , search = HttpView.empty
    }
  , [ searchAndPushUrl
    , fill
    ] |> Transition.batch
  )

encodeQuery : Model -> QueryEncode.Value
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

decodeQuery : List String -> QueryDecode.Value -> Model -> Model
decodeQuery names value model =
  { model
  | form = model.form |> View.decodeForm (names ++ ["q"]) value
  , page = value |> QueryDecode.entryAt (names ++ ["page"]) QueryDecode.int |> Maybe.withDefault model.page
  , sort =
    ( value |> QueryDecode.entryAt (names ++ ["sort","column"]) QueryDecode.string
    , value |> QueryDecode.entryAt (names ++ ["sort","order"])  QueryDecode.string
    ) |> Sort.fromString |> Maybe.withDefault model.sort
  }

encodeStore : Model -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model -> Model
decodeStore value model = model

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track signature get SearchStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput  prop value -> ( { model | form = model.form |> Form.set prop value },    Transition.none )
    FieldToggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, Transition.none )
    FieldChange -> ( model, Frame.storeApp )

    PageTo page   -> ( { model | page = page |> toPage }, searchAndPushUrl )
    SortBy sort   -> ( { model | sort = sort },           searchAndPushUrl )
    SearchRequest -> ( { model | page = 0 },              searchAndPushUrl )
    SearchStateChanged mig -> ( { model | search = model.search |> HttpView.update mig }, Transition.none )

toPage : String -> Int
toPage = String.toInt >> Maybe.withDefault 0

fill : FrameTransition a
fill = Frame.app >> .search >>
  (\model -> Dom.fill
    [ model.form.name          |> Dom.string
    , model.form.age_gteq      |> Dom.string
    , model.form.age_lteq      |> Dom.string
    , model.form.email         |> Dom.string
    , model.form.tel           |> Dom.string
    , model.form.birthday_gteq |> Dom.string
    , model.form.birthday_lteq |> Dom.string
    , model.form.start_at_gteq |> Dom.string
    , model.form.start_at_lteq |> Dom.string
    ]
  )

searchAndPushUrl : FrameTransition a
searchAndPushUrl =
  [ Http.request signature get SearchStateChanged
  , Frame.pushUrl
  ] |> Transition.batch

contents : FrameModel a -> List (Html Msg)
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

search : FrameModel a -> Html Msg
search model = L.lazy
  (\m -> Html.search
    { view = m.form |> View.view
    , http = m.search
    , options =
      { gender =
        [ ( "", "select-nothing" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      , roles =
        [ ( "admin",  "admin"  |> AppI18n.role )
        , ( "upload", "upload" |> AppI18n.role )
        ]
      }
    , msg =
      { search = SearchRequest
      , input  = FieldInput
      , toggle = FieldToggle
      , change = FieldChange
      }
    , i18n =
      { field = I18n.field
      , form  = AppI18n.form
      , http  = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .search)

paging : FrameModel a -> Html Msg
paging model = L.lazy
  (\m -> Html.paging
    { page = m.page
    , http = m.search
    , msg =
      { page = PageTo
      }
    , i18n =
      { paging = AppI18n.paging
      }
    }
  )
  (model |> Frame.app |> .search)

table : FrameModel a -> Html Msg
table model = L.lazy
  (\m -> Html.table
    { http = m.search
    , sort = m.sort
    , msg =
      { sort = SortBy
      }
    , i18n =
      { field = I18n.field
      , table = AppI18n.table
      , form  = AppI18n.form
      }
    }
  )
  (model |> Frame.app |> .search)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
