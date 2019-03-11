module GettoUpload.App.Upload.New.Register exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , encodeQuery
  , decodeQuery
  , subscriptions
  , update
  , contents
  )
import GettoUpload.App.Upload.New.Model as Model
import GettoUpload.App.Upload.New.Register.View as View
import GettoUpload.App.Upload.New.Register.Html as Html
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

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

import File exposing ( File )
import File.Select
import Json.Encode as Encode
import Json.Decode as Decode

import Browser.Navigation as Navigation
import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Html.Lazy as L

type Msg
  = FieldInput  (View.Prop String)       String
  | FieldToggle (View.Prop (Set String)) String
  | FileRequest (View.Prop (List File))
  | FileSelect  (View.Prop (List File)) File
  | FieldChange
  | UploadRequest
  | UploadStateChanged (HttpView.Migration View.Response)

signature = "register"

upload : Http.Tracker Model.Frame View.Response
upload = Http.tracker "upload" <|
  \model ->
    let
      m = model |> Frame.app |> .register
    in
      Http.upload
        { url     = "upload" |> Api.url []
        , headers = model  |> Api.headers
        , params  = m.form |> View.params
        , response = View.response
        , timeout = 30 * 1000
        }


init : Frame.InitModel -> ( Model.Register, Model.Transition Msg )
init model =
  ( { tmpId  = Nothing
    , form   = View.init signature
    , upload = HttpView.empty
    }
  , [ Frame.pushUrl
    , Frame.storeApp
    , fill
    ] |> T.batch
  )

encodeQuery : Model.Register -> QueryEncode.Value
encodeQuery model =
  case model.tmpId of
    Nothing    -> QueryEncode.empty
    Just tmpId -> [ ( "tmpId", tmpId |> String.fromInt |> QueryEncode.string ) ] |> QueryEncode.object

decodeQuery : List String -> QueryDecode.Value -> Model.Register -> Model.Register
decodeQuery names value model =
  let
    entryAt name = QueryDecode.entryAt (names ++ [name])
  in
    { model | tmpId = value |> entryAt "tmpId" QueryDecode.int }

encodeStore : Model.Register -> Encode.Value
encodeStore model =
  case model.tmpId of
    Nothing -> Encode.null
    Just tmpId -> Encode.object
      [ ( "lastTmpId", tmpId |> Encode.int )
      , ( tmpId |> String.fromInt
        , model.form |> View.encodeForm
        )
      ]

decodeStore : Decode.Value -> Model.Register -> Model.Register
decodeStore value model =
  case model.tmpId of
    Nothing ->
      let
        lastTmpId = value |> SafeDecode.at ["lastTmpId"] (SafeDecode.int 0)
      in
        { model | tmpId = lastTmpId + 1 |> Just }

    Just tmpId ->
      { model
      | form = model.form |> View.decodeForm
        (value |> SafeDecode.valueAt [tmpId |> String.fromInt])
      }

fill : Model.Transition Msg
fill = Frame.app >> .register >> .form >> Html.pairs >> Dom.fill

subscriptions : Model.Register -> Sub Msg
subscriptions model =
  Http.track signature upload UploadStateChanged

update : Msg -> Model.Register -> ( Model.Register, Model.Transition Msg )
update msg model =
  case msg of
    FieldInput  prop value -> ( { model | form = model.form |> Form.set prop value },    T.none )
    FieldToggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, T.none )
    FieldChange -> ( model, Frame.storeApp )

    FileRequest prop      -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect  prop file -> ( { model | form = model.form |> Form.set prop [file] }, T.none )

    UploadRequest -> ( model, Http.request signature upload UploadStateChanged )
    UploadStateChanged mig ->
      ( { model | upload = model.upload |> HttpView.update mig }
      , case mig |> HttpView.isSuccess of
        Just res ->
          [ Frame.clearApp
          , always ( res |> HttpView.header |> .id |> Upload.edit |> Href.toString |> Navigation.load )
          ]
          |> T.batch
        Nothing -> T.none
      )

contents : Model.Frame -> List (Html Msg)
contents model =
  [ H.section [ A.class "edit" ]
    [ model |> register
    ]
  ]

register : Model.Frame -> Html Msg
register model = L.lazy
  (\m -> Html.register
    { view   = m.form |> View.view
    , upload = m.upload
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
