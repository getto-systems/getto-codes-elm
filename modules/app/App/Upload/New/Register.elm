module GettoUpload.App.Upload.New.Register exposing
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
import GettoUpload.App.Upload.New.Register.View as View
import GettoUpload.App.Upload.New.Register.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload.New.Register as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Http.Part as Part
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field

import Browser.Navigation as Navigation
import File exposing ( File )
import File.Select
import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | register : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { signature : String
  , form : View.Form
  , upload : HttpView.Model View.ResponseHeader View.ResponseBody
  }

type Msg
  = FieldInput (View.Prop String) String
  | FieldCheck (View.Prop (Set String)) String
  | FieldChange
  | FileRequest (View.Prop (List File))
  | FileSelect (View.Prop (List File)) File
  | UploadRequest
  | UploadStateChanged (HttpView.Migration View.ResponseHeader View.ResponseBody)

init : String -> Frame.InitModel -> ( Model, FrameTransition a )
init signature model =
  ( { signature = signature
    , form =
      { name     = Field.init signature "name"     ""
      , text     = Field.init signature "text"     []
      , memo     = Field.init signature "memo"     ""
      , age      = Field.init signature "age"      ""
      , email    = Field.init signature "email"    ""
      , tel      = Field.init signature "tel"      ""
      , birthday = Field.init signature "birthday" ""
      , start_at = Field.init signature "start_at" ""
      , gender   = Field.init signature "gender"   ""
      , quality  = Field.init signature "quality"  ""
      , roles    = Field.init signature "roles"    Set.empty
      }
    , upload = HttpView.empty
    }
  , fill
  )

fill : FrameModel a -> Cmd msg
fill = Frame.app >> .register >>
  (\m -> Dom.fill
    [ m.form.name     |> Dom.string
    , m.form.memo     |> Dom.string
    , m.form.age      |> Dom.string
    , m.form.email    |> Dom.string
    , m.form.tel      |> Dom.string
    , m.form.birthday |> Dom.string
    , m.form.start_at |> Dom.string
    , m.form.gender   |> Dom.string
    ]
  )

upload : Http.Payload (FrameModel a) View.ResponseHeader View.ResponseBody
upload = Http.payload "upload" <|
  \model ->
    let
      m = model |> Frame.app |> .register
    in
    Http.upload
      { url     = "upload" |> Api.url []
      , headers = model |> Api.headers
      , params  = Part.object
        [ ( "name",      m.form.name     |> Field.value |> Part.string )
        , ( "text" ,     m.form.text     |> Field.value |> Part.list Part.file )
        , ( "memo",      m.form.memo     |> Field.value |> Part.string )
        , ( "age",       m.form.age      |> Field.value |> Part.string )
        , ( "email",     m.form.email    |> Field.value |> Part.string )
        , ( "tel",       m.form.tel      |> Field.value |> Part.string )
        , ( "birthday",  m.form.birthday |> Field.value |> Part.string )
        , ( "start_at",  m.form.start_at |> Field.value |> Part.string )
        , ( "gender",    m.form.gender   |> Field.value |> Part.string )
        , ( "quality",   m.form.quality  |> Field.value |> Part.string )
        , ( "roles",     m.form.roles    |> Field.value |> Set.toList |> Part.list Part.string )
        ]
      , response =
        { header = HeaderDecode.map View.ResponseHeader
          ( HeaderDecode.at "x-upload-id" HeaderDecode.int )
        , body = Decode.succeed ()
        }
      , timeout = 30 * 1000
      }

query : Model -> QueryEncode.Value
query model = QueryEncode.empty

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model = model

store : Model -> Encode.Value
store model = Encode.object
  [ ( "name",     model.form.name     |> Field.value |> Encode.string )
  , ( "memo",     model.form.memo     |> Field.value |> Encode.string )
  , ( "age",      model.form.age      |> Field.value |> Encode.string )
  , ( "email",    model.form.email    |> Field.value |> Encode.string )
  , ( "tel",      model.form.tel      |> Field.value |> Encode.string )
  , ( "birthday", model.form.birthday |> Field.value |> Encode.string )
  , ( "start_at", model.form.start_at |> Field.value |> Encode.string )
  , ( "gender",   model.form.gender   |> Field.value |> Encode.string )
  , ( "quality",  model.form.quality  |> Field.value |> Encode.string )
  , ( "roles",    model.form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
  ]

storeChanged : Decode.Value -> Model -> Model
storeChanged value model =
  { model
  | form =
    model.form
    |> View.update name_     ( value |> SafeDecode.at ["name"]     (SafeDecode.string "") )
    |> View.update memo_     ( value |> SafeDecode.at ["memo"]     (SafeDecode.string "") )
    |> View.update age_      ( value |> SafeDecode.at ["age"]      (SafeDecode.string "") )
    |> View.update email_    ( value |> SafeDecode.at ["email"]    (SafeDecode.string "") )
    |> View.update tel_      ( value |> SafeDecode.at ["tel"]      (SafeDecode.string "") )
    |> View.update birthday_ ( value |> SafeDecode.at ["birthday"] (SafeDecode.string "") )
    |> View.update start_at_ ( value |> SafeDecode.at ["start_at"] (SafeDecode.string "") )
    |> View.update gender_   ( value |> SafeDecode.at ["gender"]   (SafeDecode.string "") )
    |> View.update quality_  ( value |> SafeDecode.at ["quality"]  (SafeDecode.string "") )
    |> View.update roles_    ( value |> SafeDecode.at ["roles"]    (SafeDecode.list (SafeDecode.string "")) |> Set.fromList )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track model.signature upload UploadStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput prop value ->
      ( { model | form = model.form |> View.update prop value }
      , Transition.none
      )
    FieldCheck prop value ->
      ( { model | form = model.form |> View.toggle prop value }
      , Transition.none
      )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect prop file ->
      ( { model | form = model.form |> View.update prop [file] }
      , Transition.none
      )

    UploadRequest -> ( model, Http.request model.signature upload UploadStateChanged )
    UploadStateChanged mig ->
      ( { model | upload = model.upload |> HttpView.update mig }
      , case mig of
        HttpView.Success res ->
          [ Frame.clearApp
          , always ( res |> HttpView.header |> .id |> Upload.edit |> Href.toString |> Navigation.load )
          ]
          |> Transition.batch
        _ -> Transition.none
      )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ H.section [ A.class "edit" ]
    [ model |> register
    ]
  ]

name_     = View.prop .name     (\v m -> { m | name     = v })
text_     = View.prop .text     (\v m -> { m | text     = v })
memo_     = View.prop .memo     (\v m -> { m | memo     = v })
age_      = View.prop .age      (\v m -> { m | age      = v })
email_    = View.prop .email    (\v m -> { m | email    = v })
tel_      = View.prop .tel      (\v m -> { m | tel      = v })
birthday_ = View.prop .birthday (\v m -> { m | birthday = v })
start_at_ = View.prop .start_at (\v m -> { m | start_at = v })
gender_   = View.prop .gender   (\v m -> { m | gender   = v })
quality_  = View.prop .quality  (\v m -> { m | quality  = v })
roles_    = View.prop .roles    (\v m -> { m | roles    = v })

register : FrameModel a -> Html Msg
register model = L.lazy
  (\m -> Html.register
    { title = "register"
    , form = m.form |> View.compose
      ( name_,     [ m.form.name |> Field.blank "blank" ] )
      ( text_,     [ m.form.text |> Field.empty "no-file" ] )
      ( memo_,     [] )
      ( age_,      [] )
      ( email_,    [] )
      ( tel_,      [] )
      ( birthday_, [] )
      ( start_at_, [] )
      ( gender_,   [] )
      ( quality_,  [] )
      ( roles_,    [] )
    , http = m.upload
    , options =
      { gender =
        [ ( "", "please-select" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      , quality =
        [ ( "high", "high" |> I18n.quality )
        , ( "low",  "low"  |> I18n.quality )
        ]
      , roles =
        [ ( "admin",  "admin"  |> AppI18n.role )
        , ( "upload", "upload" |> AppI18n.role )
        ]
      }
    , msg =
      { upload = UploadRequest
      , input  = FieldInput
      , check  = FieldCheck
      , change = FieldChange
      , select = FileRequest
      }
    , i18n =
      { title = I18n.title
      , field = I18n.field
      , error = I18n.error
      , form  = AppI18n.form
      , http  = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .register)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
