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
import GettoUpload.App.Upload.ListEdit.Info   as Info
import GettoUpload.App.Upload.ListEdit.Detail as Detail
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Options.View as Options
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
  | Detail Detail.Msg
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
      search = model |> Frame.app |> .search
    in
      Http.get
        { url     = "uploads" |> Api.url []
        , headers = model |> Api.headers
        , params  = QueryEncode.object
          [ ( "q"
            , [ ( "name",   search.form.name   |> Field.value |> QueryEncode.string )
              , ( "gender", search.form.gender |> Field.value |> QueryEncode.string )
              ] |> QueryEncode.object
            )
          , ( "page", search.page |> QueryEncode.int )
          , ( "sort"
            , case search.sort |> Sort.expose of
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
  ( { form   = View.init signature
    , page   = 0
    , sort   = "id" |> Sort.by
    , get    = HttpView.empty
    , info   = Info.init signature
    , detail = Nothing
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

info_   = T.prop .info   (\v m -> { m | info   = v })
detail_ = T.prop .detail (\v m -> { m | detail = v })

update : Msg -> Model.Search -> ( Model.Search, Model.Transition Msg )
update message model =
  case message of
    Info   msg -> model |> T.update info_   (Info.update msg   >> T.map Info)
    Detail msg -> model |> T.update detail_ (Detail.update msg >> T.map Detail)

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
      [ model |> form
      ]
    , H.section [ "data" |> A.class ]
      [ model |> paging
      , model |> table
      , model |> paging
      ]
    ]
  ]

form : Model.Frame -> Html Msg
form model = L.lazy2
  (\options search -> Html.search
    { view = search.form |> View.view
    , get  = search.get
    , options =
      { gender = options.get |> Options.gender |> toSelectOptions I18n.gender
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
  (model |> Frame.layout |> .options)
  (model |> Frame.app    |> .search)

paging : Model.Frame -> Html Msg
paging model = L.lazy
  (\search -> Html.paging
    { page = search.page
    , get  = search.get
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
  (\search -> Html.table
    { get  = search.get
    , sort = search.sort
    , info = search.info |> Info.content
    , msg =
      { sort = SortBy
      , info = Info
      , edit = Detail.edit >> Detail
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
dialogs model =
  [ model |> Detail.content |> H.map Detail
  ]

toSelectOptions : (String -> String) -> Maybe (List String) -> List ( String, String )
toSelectOptions i18n = Maybe.map (toOptions i18n >> includeDefault) >> withLoading

toBoxOptions : (String -> String) -> Maybe (List String) -> List ( String, String )
toBoxOptions i18n = Maybe.map (toOptions i18n) >> Maybe.withDefault []

toOptions : (String -> String) -> List String -> List ( String, String )
toOptions i18n = List.map (\value -> ( value, value |> i18n ) )

includeDefault : List ( String, String ) -> List ( String, String )
includeDefault options = ( "", "please-select" |> AppI18n.form ) :: options

withLoading : Maybe (List ( String, String )) -> List ( String, String )
withLoading = Maybe.withDefault [ ( "", "loading" |> AppI18n.form ) ]
