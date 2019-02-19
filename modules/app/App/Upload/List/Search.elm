module GettoUpload.App.Upload.List.Search exposing
  ( Model
  , Msg
  , init
  , query
  , queryChanged
  , store
  , storeChanged
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
import GettoUpload.I18n.App.Upload.List.Search as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present

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
  { signature : String
  , form      : View.Form
  , search    : HttpView.Model View.ResponseHeader View.ResponseBody
  }

type Msg
  = FieldInput (Form.Prop View.Form String) String
  | FieldCheck (Form.Prop View.Form (Set String)) String
  | FieldChange
  | SearchRequest
  | SearchStateChanged (HttpView.Migration View.ResponseHeader View.ResponseBody)

init : String -> Frame.InitModel -> ( Model, FrameTransition a )
init signature model =
  ( { signature = signature
    , form =
      { name          = Field.init signature "name"          ""
      , age_gteq      = Field.init signature "age_gteq"      ""
      , age_lteq      = Field.init signature "age_lteq"      ""
      , email         = Field.init signature "email"         ""
      , tel           = Field.init signature "tel"           ""
      , birthday_gteq = Field.init signature "birthday_gteq" ""
      , birthday_lteq = Field.init signature "birthday_lteq" ""
      , start_at_gteq = Field.init signature "start_at_gteq" ""
      , start_at_lteq = Field.init signature "start_at_lteq" ""
      , gender        = Field.init signature "gender"        ""
      , roles         = Field.init signature "roles"         Set.empty
      }
    , search = HttpView.empty
    }
  , [ fill
    , Http.request signature search SearchStateChanged
    ] |> Transition.batch
  )

fill : FrameModel a -> Cmd msg
fill = Frame.app >> .search >>
  (\m -> Dom.fill
    [ m.form.name          |> Dom.string
    , m.form.age_gteq      |> Dom.string
    , m.form.age_lteq      |> Dom.string
    , m.form.email         |> Dom.string
    , m.form.tel           |> Dom.string
    , m.form.birthday_gteq |> Dom.string
    , m.form.birthday_lteq |> Dom.string
    , m.form.start_at_gteq |> Dom.string
    , m.form.start_at_lteq |> Dom.string
    , m.form.gender        |> Dom.string
    ]
  )

search : Http.Tracker (FrameModel a) View.ResponseHeader View.ResponseBody
search = Http.tracker "search" <|
  \model ->
    let
      m = model |> Frame.app |> .search
    in
      Http.get
        { url     = "uploads" |> Api.url []
        , headers = model |> Api.headers
        , params  = m |> query
        , response =
          { header = HeaderDecode.succeed ()
          , body = Decode.succeed ()
          }
        , timeout = 10 * 1000
        }

query : Model -> QueryEncode.Value
query model = QueryEncode.object
  [ ( "[ ]=&?",           model.form.name          |> Field.value |> QueryEncode.string )
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
  ]

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model =
  let
    entryAt name = QueryDecode.entryAt (names ++ [name]) (QueryDecode.string "")
    listAt  name = QueryDecode.listAt  (names ++ [name]) (QueryDecode.string "")
  in
    { model
    | form =
      model.form
      |> Form.set name_          ( value |> entryAt "[ ]=&?"          )
      |> Form.set age_gteq_      ( value |> entryAt "age_gteq"      )
      |> Form.set age_lteq_      ( value |> entryAt "age_lteq"      )
      |> Form.set email_         ( value |> entryAt "email"         )
      |> Form.set tel_           ( value |> entryAt "tel"           )
      |> Form.set birthday_gteq_ ( value |> entryAt "birthday_gteq" )
      |> Form.set birthday_lteq_ ( value |> entryAt "birthday_lteq" )
      |> Form.set start_at_gteq_ ( value |> entryAt "start_at_gteq" )
      |> Form.set start_at_lteq_ ( value |> entryAt "start_at_lteq" )
      |> Form.set gender_        ( value |> entryAt "gender"        )
      |> Form.set roles_         ( value |> listAt  "roles" |> Set.fromList )
    }

store : Model -> Encode.Value
store model = Encode.null

storeChanged : Decode.Value -> Model -> Model
storeChanged value model = model

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track model.signature search SearchStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput prop value ->
      ( { model | form = model.form |> Form.set prop value }
      , Transition.none
      )
    FieldCheck prop value ->
      ( { model | form = model.form |> Form.toggle prop value }
      , Transition.none
      )
    FieldChange -> ( model, Frame.storeApp )

    SearchRequest ->
      ( model
      , [ Http.request model.signature search SearchStateChanged
        , Frame.pushUrl
        ] |> Transition.batch
      )
    SearchStateChanged mig -> ( { model | search = model.search |> HttpView.update mig }, Transition.none )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ H.section [ A.class "list" ]
    [ model |> contentSearch
    , model |> contentData
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

contentSearch : FrameModel a -> Html Msg
contentSearch model = L.lazy
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
      , check  = FieldCheck
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

contentData : FrameModel a -> Html Msg
contentData model = L.lazy
  (\s ->
    "" |> H.text
  )
  (model |> Frame.app |> .search)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
