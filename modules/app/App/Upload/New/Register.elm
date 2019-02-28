module GettoUpload.App.Upload.New.Register exposing
  ( Model
  , Msg
  , init
  , encodeStore
  , decodeStore
  , encodeQuery
  , decodeQuery
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
import GettoUpload.I18n.App.Upload as I18n
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
  { tmpId  : Maybe Int
  , form   : View.Form
  , upload : HttpView.Model View.Response
  }

type Msg
  = FieldInput  (View.Prop String)       String
  | FieldToggle (View.Prop (Set String)) String
  | FileRequest (View.Prop (List File))
  | FileSelect  (View.Prop (List File)) File
  | FieldChange
  | UploadRequest
  | UploadStateChanged (HttpView.Migration View.Response)

signature = "register"

upload : Http.Tracker (FrameModel a) View.Response
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
        , response = HttpView.decoder
          { header = HeaderDecode.map View.ResponseHeader
            ( HeaderDecode.at "x-upload-id" HeaderDecode.int )
          , body = Decode.succeed ()
          }
        , timeout = 30 * 1000
        }


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { tmpId = Nothing
    , form =
      { name     = Field.init signature "name"     () ""
      , text     = Field.init signature "text"     () []
      , memo     = Field.init signature "memo"     () ""
      , age      = Field.init signature "age"      () ""
      , email    = Field.init signature "email"    () ""
      , tel      = Field.init signature "tel"      () ""
      , birthday = Field.init signature "birthday" () ""
      , start_at = Field.init signature "start_at" () ""
      , gender   = Field.init signature "gender"   () ""
      , quality  = Field.init signature "quality"  () ""
      , roles    = Field.init signature "roles"    () Set.empty
      }
    , upload = HttpView.empty
    }
  , [ Frame.pushUrl
    , Frame.storeApp
    , fill
    ] |> Transition.batch
  )

encodeQuery : Model -> QueryEncode.Value
encodeQuery model =
  case model.tmpId of
    Nothing    -> QueryEncode.empty
    Just tmpId -> [ ( "tmpId", tmpId |> String.fromInt |> QueryEncode.string ) ] |> QueryEncode.object

decodeQuery : List String -> QueryDecode.Value -> Model -> Model
decodeQuery names value model =
  let
    entryAt name = QueryDecode.entryAt (names ++ [name])
  in
    { model | tmpId = value |> entryAt "tmpId" QueryDecode.int }

encodeStore : Model -> Encode.Value
encodeStore model =
  case model.tmpId of
    Nothing -> Encode.null
    Just tmpId -> Encode.object
      [ ( "lastTmpId", tmpId |> Encode.int )
      , ( tmpId |> String.fromInt
        , [ ( "name",     model.form.name     |> Field.value |> Encode.string )
          , ( "memo",     model.form.memo     |> Field.value |> Encode.string )
          , ( "age",      model.form.age      |> Field.value |> Encode.string )
          , ( "email",    model.form.email    |> Field.value |> Encode.string )
          , ( "tel",      model.form.tel      |> Field.value |> Encode.string )
          , ( "birthday", model.form.birthday |> Field.value |> Encode.string )
          , ( "start_at", model.form.start_at |> Field.value |> Encode.string )
          , ( "gender",   model.form.gender   |> Field.value |> Encode.string )
          , ( "quality",  model.form.quality  |> Field.value |> Encode.string )
          , ( "roles",    model.form.roles    |> Field.value |> Set.toList |> Encode.list Encode.string )
          ] |> Encode.object
        )
      ]

decodeStore : Decode.Value -> Model -> Model
decodeStore value model =
  case model.tmpId of
    Nothing ->
      let
        lastTmpId = value |> SafeDecode.at ["lastTmpId"] (SafeDecode.int 0)
      in
        { model | tmpId = lastTmpId + 1 |> Just }

    Just tmpId ->
      let
        obj = value |> SafeDecode.valueAt [tmpId |> String.fromInt]
        decode name decoder = Decode.decodeValue (Decode.at [name] decoder) >> Result.toMaybe
      in
        { model
        | form =
          model.form
          |> Form.setIf name_     ( obj |> decode "name"     Decode.string )
          |> Form.setIf memo_     ( obj |> decode "memo"     Decode.string )
          |> Form.setIf age_      ( obj |> decode "age"      Decode.string )
          |> Form.setIf email_    ( obj |> decode "email"    Decode.string )
          |> Form.setIf tel_      ( obj |> decode "tel"      Decode.string )
          |> Form.setIf birthday_ ( obj |> decode "birthday" Decode.string )
          |> Form.setIf start_at_ ( obj |> decode "start_at" Decode.string )
          |> Form.setIf gender_   ( obj |> decode "gender"   Decode.string )
          |> Form.setIf quality_  ( obj |> decode "quality"  Decode.string )
          |> Form.setIf roles_    ( obj |> decode "roles"  ((Decode.list Decode.string) |> Decode.map Set.fromList) )
        }

fill : FrameTransition a
fill = Frame.app >> .register >>
  (\model -> Dom.fill
    [ model.form.name     |> Dom.string
    , model.form.memo     |> Dom.string
    , model.form.age      |> Dom.string
    , model.form.email    |> Dom.string
    , model.form.tel      |> Dom.string
    , model.form.birthday |> Dom.string
    , model.form.start_at |> Dom.string
    ]
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track signature upload UploadStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    FieldInput  prop value -> ( { model | form = model.form |> Form.set prop value },    Transition.none )
    FieldToggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, Transition.none )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop      -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect  prop file -> ( { model | form = model.form |> Form.set prop [file] }, Transition.none )

    UploadRequest -> ( model, Http.request signature upload UploadStateChanged )
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
      , toggle = FieldToggle
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
