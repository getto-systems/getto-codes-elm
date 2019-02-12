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
import Json.Encode as Encode
import Json.Decode as Decode
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | register : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form : View.Form
  , upload : Http.Model Upload Decode.Value
  }

type alias Upload =
  { id : Int
  }

type Msg
  = FieldInput (View.Prop String) String
  | FieldChange
  | FileRequest (View.Prop (List File))
  | FileSelect (View.Prop (List File)) File
  | UploadRequest
  | UploadStateChanged (HttpView.State Upload Decode.Value)

init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form =
      { name     = Field.init "register" "name"     ""
      , text     = Field.init "register" "text"     []
      , memo     = Field.init "register" "memo"     ""
      , age      = Field.init "register" "age"      ""
      , email    = Field.init "register" "email"    ""
      , tel      = Field.init "register" "tel"      ""
      , birthday = Field.init "register" "birthday" ""
      , start_at = Field.init "register" "start_at" ""
      , gender   = Field.init "register" "gender"   ""
      }
    , upload = Http.init
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

upload : Http.Request (FrameModel a) Upload Decode.Value
upload = Api.request
  (\(host,headers) ->
    Http.upload
      { url     = host ++ "upload"
      , headers = headers
      , params  = \model ->
        let
          m = model |> Frame.app |> .register
        in
          Part.object
            [ ( "name",      m.form.name     |> Field.value |> Part.string )
            , ( "text" ,     m.form.text     |> Field.value |> Part.list Part.file )
            , ( "memo",      m.form.memo     |> Field.value |> Part.string )
            , ( "age",       m.form.age      |> Field.value |> Part.string )
            , ( "email",     m.form.email    |> Field.value |> Part.string )
            , ( "tel",       m.form.tel      |> Field.value |> Part.string )
            , ( "birthday",  m.form.birthday |> Field.value |> Part.string )
            , ( "start_at",  m.form.start_at |> Field.value |> Part.string )
            , ( "gender",    m.form.gender   |> Field.value |> Part.string )
            ]
      , response =
        { header = HeaderDecode.map Upload
          ( HeaderDecode.at "x-upload-id" HeaderDecode.int )
        , body = Decode.value
        }
      , timeout = 30 * 1000
      }
  )

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
  ]

storeChanged : Decode.Value -> Model -> Model
storeChanged value model =
  { model
  | form =
    model.form
    |> View.change name_     ( value |> SafeDecode.at ["name"]     (SafeDecode.string "") )
    |> View.change memo_     ( value |> SafeDecode.at ["memo"]     (SafeDecode.string "") )
    |> View.change age_      ( value |> SafeDecode.at ["age"]      (SafeDecode.string "") )
    |> View.change email_    ( value |> SafeDecode.at ["email"]    (SafeDecode.string "") )
    |> View.change tel_      ( value |> SafeDecode.at ["tel"]      (SafeDecode.string "") )
    |> View.change birthday_ ( value |> SafeDecode.at ["birthday"] (SafeDecode.string "") )
    |> View.change start_at_ ( value |> SafeDecode.at ["start_at"] (SafeDecode.string "") )
    |> View.change gender_   ( value |> SafeDecode.at ["gender"]   (SafeDecode.string "") )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track upload UploadStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput prop value ->
      ( { model | form = model.form |> View.change prop value }
      , Transition.none
      )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect prop file ->
      ( { model | form = model.form |> View.change prop [file] }
      , Transition.none
      )

    UploadRequest -> ( model, Http.request upload UploadStateChanged )
    UploadStateChanged state ->
      ( { model | upload = model.upload |> Http.stateTo state }
      , case state of
        HttpView.Ready (Just (Ok res)) ->
          [ Frame.clearApp
          , always ( res |> HttpView.header |> .id |> Upload.edit |> Href.toString |> Navigation.load )
          ]
          |> Transition.batch
        _ -> Transition.none
      )

contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> list
  ]

list : FrameModel a -> Html Msg
list model =
  H.section [ A.class "edit" ]
    [ model |> register
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
    , state = m.upload |> Http.state
    , options =
      { gender =
        [ ( "", "please-select" |> AppI18n.form )
        , ( "male",   "male"   |> I18n.gender )
        , ( "female", "female" |> I18n.gender )
        , ( "other",  "other"  |> I18n.gender )
        ]
      }
    , msg =
      { upload = UploadRequest
      , input  = FieldInput
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
