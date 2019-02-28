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
        , params  = m |> encodeQuery
        , response = HttpView.decoder
          { header = HeaderDecode.map View.ResponseHeader
            ( HeaderDecode.at "x-paging-max" HeaderDecode.int )
          , body = Decode.list
            ( Decode.map5 View.Upload
              ( Decode.at ["id"]        Decode.int )
              ( Decode.at ["name"]      Decode.string )
              ( Decode.at ["gender"]    Decode.string )
              ( Decode.at ["roles"]    (Decode.list Decode.string) )
              ( Decode.at ["comments"] (Decode.list (Decode.map3 View.Comment
                ( Decode.at ["user"]   Decode.string )
                ( Decode.at ["text"]   Decode.string )
                ( Decode.at ["likes"] (Decode.list (Decode.map2 View.Like
                  ( Decode.at ["user"]   Decode.string )
                  ( Decode.at ["text"]   Decode.string )
                )) )
              )) )
            )
          }
        , timeout = 10 * 1000
        }


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form =
      { name          = Field.init signature "name"          () ""
      , age_gteq      = Field.init signature "age_gteq"      () ""
      , age_lteq      = Field.init signature "age_lteq"      () ""
      , email         = Field.init signature "email"         () ""
      , tel           = Field.init signature "tel"           () ""
      , birthday_gteq = Field.init signature "birthday_gteq" () ""
      , birthday_lteq = Field.init signature "birthday_lteq" () ""
      , start_at_gteq = Field.init signature "start_at_gteq" () ""
      , start_at_lteq = Field.init signature "start_at_lteq" () ""
      , gender        = Field.init signature "gender"        () ""
      , roles         = Field.init signature "roles"         () Set.empty
      }
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
  [ ( "q"
    , [ ( "name",           model.form.name          |> Field.value |> QueryEncode.string )
      , ( "age_gteq",       model.form.age_gteq      |> Field.value |> QueryEncode.string )
      , ( "age_lteq",       model.form.age_lteq      |> Field.value |> QueryEncode.string )
      , ( "email",          model.form.email         |> Field.value |> QueryEncode.string )
      , ( "tel",            model.form.tel           |> Field.value |> QueryEncode.string )
      , ( "birthday_gteq",  model.form.birthday_gteq |> Field.value |> QueryEncode.string )
      , ( "birthday_lteq",  model.form.birthday_lteq |> Field.value |> QueryEncode.string )
      , ( "start_at_gteq",  model.form.start_at_gteq |> Field.value |> QueryEncode.string )
      , ( "start_at_lteq",  model.form.start_at_lteq |> Field.value |> QueryEncode.string )
      , ( "gender",         model.form.gender        |> Field.value |> QueryEncode.string )
      , ( "roles",          model.form.roles         |> Field.value |> Set.toList |> QueryEncode.list QueryEncode.string )
      ] |> QueryEncode.object
    )
  , ( "p", model.page |> QueryEncode.int )
  , ( "s"
    , case model.sort |> Sort.expose of
      (column,order) ->
        [ ( "column", column |> QueryEncode.string )
        , ( "order",  order  |> QueryEncode.string )
        ] |> QueryEncode.object
    )
  ]

decodeQuery : List String -> QueryDecode.Value -> Model -> Model
decodeQuery names value model =
  let
    qEntryAt name = QueryDecode.entryAt (names ++ ["q",name]) QueryDecode.string
    qListAt  name = QueryDecode.listAt  (names ++ ["q",name]) QueryDecode.string
  in
    { model
    | form =
      model.form
      |> Form.setIf name_          ( value |> qEntryAt "name"          )
      |> Form.setIf age_gteq_      ( value |> qEntryAt "age_gteq"      )
      |> Form.setIf age_lteq_      ( value |> qEntryAt "age_lteq"      )
      |> Form.setIf email_         ( value |> qEntryAt "email"         )
      |> Form.setIf tel_           ( value |> qEntryAt "tel"           )
      |> Form.setIf birthday_gteq_ ( value |> qEntryAt "birthday_gteq" )
      |> Form.setIf birthday_lteq_ ( value |> qEntryAt "birthday_lteq" )
      |> Form.setIf start_at_gteq_ ( value |> qEntryAt "start_at_gteq" )
      |> Form.setIf start_at_lteq_ ( value |> qEntryAt "start_at_lteq" )
      |> Form.setIf gender_        ( value |> qEntryAt "gender"        )
      |> Form.setIf roles_         ( value |> qListAt  "roles" |> Maybe.map Set.fromList )
    , page = value |> QueryDecode.entryAt (names ++ ["p"]) QueryDecode.int |> Maybe.withDefault model.page
    , sort =
      ( value |> QueryDecode.entryAt (names ++ ["s","column"]) QueryDecode.string
      , value |> QueryDecode.entryAt (names ++ ["s","order"])  QueryDecode.string
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

name_          = Form.prop .name          (\v m -> { m | name          = v })
age_gteq_      = Form.prop .age_gteq      (\v m -> { m | age_gteq      = v })
age_lteq_      = Form.prop .age_lteq      (\v m -> { m | age_lteq      = v })
email_         = Form.prop .email         (\v m -> { m | email         = v })
tel_           = Form.prop .tel           (\v m -> { m | tel           = v })
birthday_gteq_ = Form.prop .birthday_gteq (\v m -> { m | birthday_gteq = v })
birthday_lteq_ = Form.prop .birthday_lteq (\v m -> { m | birthday_lteq = v })
start_at_gteq_ = Form.prop .start_at_gteq (\v m -> { m | start_at_gteq = v })
start_at_lteq_ = Form.prop .start_at_lteq (\v m -> { m | start_at_lteq = v })
gender_        = Form.prop .gender        (\v m -> { m | gender        = v })
roles_         = Form.prop .roles         (\v m -> { m | roles         = v })

search : FrameModel a -> Html Msg
search model = L.lazy
  (\m -> Html.search
    { form = m.form |> View.compose
      { name          = ( name_,          Present.string )
      , age_gteq      = ( age_gteq_,      Present.string )
      , age_lteq      = ( age_lteq_,      Present.string )
      , email         = ( email_,         Present.string )
      , tel           = ( tel_,           Present.string )
      , birthday_gteq = ( birthday_gteq_, Present.string )
      , birthday_lteq = ( birthday_lteq_, Present.string )
      , start_at_gteq = ( start_at_gteq_, Present.string )
      , start_at_lteq = ( start_at_lteq_, Present.string )
      , gender        = ( gender_,        Present.string )
      , roles         = ( roles_,         Present.set )
      }
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
