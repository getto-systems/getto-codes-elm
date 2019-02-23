module GettoUpload.App.Upload.New.Register exposing
  ( Model
  , Msg
  , init
  , store
  , storeChanged
  , query
  , queryChanged
  , fill
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
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

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
  , form      : View.Form
  , upload    : HttpView.Model View.ResponseHeader View.ResponseBody
  }

type Msg
  = FieldInput (Form.Prop View.Form String) String
  | FieldCheck (Form.Prop View.Form (Set String)) String
  | FieldChange
  | FileRequest (Form.Prop View.Form (List File))
  | FileSelect (Form.Prop View.Form (List File)) File
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
  , Transition.none
  )

upload : Http.Tracker (FrameModel a) View.ResponseHeader View.ResponseBody
upload = Http.tracker "upload" <|
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
    |> Form.set name_     ( value |> SafeDecode.at ["name"]     (SafeDecode.string "") )
    |> Form.set memo_     ( value |> SafeDecode.at ["memo"]     (SafeDecode.string "") )
    |> Form.set age_      ( value |> SafeDecode.at ["age"]      (SafeDecode.string "") )
    |> Form.set email_    ( value |> SafeDecode.at ["email"]    (SafeDecode.string "") )
    |> Form.set tel_      ( value |> SafeDecode.at ["tel"]      (SafeDecode.string "") )
    |> Form.set birthday_ ( value |> SafeDecode.at ["birthday"] (SafeDecode.string "") )
    |> Form.set start_at_ ( value |> SafeDecode.at ["start_at"] (SafeDecode.string "") )
    |> Form.set gender_   ( value |> SafeDecode.at ["gender"]   (SafeDecode.string "") )
    |> Form.set quality_  ( value |> SafeDecode.at ["quality"]  (SafeDecode.string "") )
    |> Form.set roles_    ( value |> SafeDecode.at ["roles"]    (SafeDecode.list (SafeDecode.string "")) |> Set.fromList )
  }

query : Model -> QueryEncode.Value
query model = QueryEncode.empty

queryChanged : List String -> QueryDecode.Value -> Model -> Model
queryChanged names value model = model

fill : Model -> List ( String, String )
fill model =
  [ model.form.name     |> Dom.string
  , model.form.memo     |> Dom.string
  , model.form.age      |> Dom.string
  , model.form.email    |> Dom.string
  , model.form.tel      |> Dom.string
  , model.form.birthday |> Dom.string
  , model.form.start_at |> Dom.string
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track model.signature upload UploadStateChanged

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

    FileRequest prop -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect prop file ->
      ( { model | form = model.form |> Form.set prop [file] }
      , Transition.none
      )

    UploadRequest -> ( model, Http.request model.signature upload UploadStateChanged )
    UploadStateChanged mig ->
      ( { model | upload = model.upload |> HttpView.update mig }
      , case mig |> HttpView.isSuccess of
        Just res ->
          [ Frame.clearApp
          , always ( res |> HttpView.header |> .id |> Upload.edit |> Href.toString |> Navigation.load )
          ]
          |> Transition.batch
        Nothing -> Transition.none
      )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ H.section [ A.class "edit" ]
    [ model |> register
    ]
  ]

name_     = Form.prop .name     (\v m -> { m | name     = v })
text_     = Form.prop .text     (\v m -> { m | text     = v })
memo_     = Form.prop .memo     (\v m -> { m | memo     = v })
age_      = Form.prop .age      (\v m -> { m | age      = v })
email_    = Form.prop .email    (\v m -> { m | email    = v })
tel_      = Form.prop .tel      (\v m -> { m | tel      = v })
birthday_ = Form.prop .birthday (\v m -> { m | birthday = v })
start_at_ = Form.prop .start_at (\v m -> { m | start_at = v })
gender_   = Form.prop .gender   (\v m -> { m | gender   = v })
quality_  = Form.prop .quality  (\v m -> { m | quality  = v })
roles_    = Form.prop .roles    (\v m -> { m | roles    = v })

register : FrameModel a -> Html Msg
register model = L.lazy
  (\m -> Html.register
    { title = "register"
    , form = m.form |> View.compose
      { name     = ( name_,     [ m.form.name |> Validate.blank "blank" ] )
      , text     = ( text_,     [ m.form.text |> Validate.empty "no-file" ] )
      , memo     = ( memo_,     [] )
      , age      = ( age_,      [] )
      , email    = ( email_,    [] )
      , tel      = ( tel_,      [] )
      , birthday = ( birthday_, [] )
      , start_at = ( start_at_, [] )
      , gender   = ( gender_,   [] )
      , quality  = ( quality_,  [] )
      , roles    = ( roles_,    [] )
      }
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
