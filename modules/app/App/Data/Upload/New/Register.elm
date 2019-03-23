module GettoCodes.App.Data.Upload.New.Register exposing
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
import GettoCodes.App.Data.Upload.New.Model as Model
import GettoCodes.App.Data.Upload.New.Register.View as View
import GettoCodes.App.Data.Upload.New.Register.Html as Html
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Api as Api
import GettoCodes.Command.Http as Http
import GettoCodes.Command.Dom as Dom
import GettoCodes.View.Http as HttpView
import GettoCodes.I18n.App as AppI18n
import GettoCodes.I18n.App.Data.Upload as I18n
import GettoCodes.I18n.Http as HttpI18n
import GettoCodes.Extension.Href as Href
import GettoCodes.Extension.Href.Data.Upload as Upload

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field.Form as Form

import File exposing ( File )
import File.Select
import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Change
  | Upload
  | Input  (View.Prop String)       String
  | Toggle (View.Prop (Set String)) String
  | FileRequest (View.Prop (List File))
  | FileSelect  (View.Prop (List File)) File
  | StateChanged (HttpView.Migration View.Response)

signature = "register"

upload : Http.Tracker Model.Frame View.Response
upload = Http.tracker "upload" <|
  \model ->
    let
      register = model |> Frame.app |> .register
    in
      Http.upload
        { url     = "upload" |> Api.url []
        , headers = model  |> Api.headers
        , params  = register.form |> View.params
        , response = View.response
        , timeout = 30 * 1000
        }

uploadTrack   = Http.track   signature upload StateChanged
uploadRequest = Http.request signature upload StateChanged


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
    Nothing    -> QueryEncode.null
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
subscriptions model = uploadTrack

update : Msg -> Model.Register -> ( Model.Register, Model.Transition Msg )
update msg model =
  case msg of
    Change -> ( model, Frame.storeApp )
    Upload -> ( model, uploadRequest )

    Input  prop value -> ( { model | form = model.form |> Form.set prop value },    T.none )
    Toggle prop value -> ( { model | form = model.form |> Form.toggle prop value }, T.none )

    FileRequest prop      -> ( model, always ( FileSelect prop |> File.Select.file [] ) )
    FileSelect  prop file -> ( { model | form = model.form |> Form.set prop [file] }, T.none )

    StateChanged mig ->
      ( { model | upload = model.upload |> HttpView.update mig }
      , case mig |> HttpView.isSuccess of
        Just res ->
          [ Frame.clearApp
          , res |> HttpView.header |> .id |> Upload.edit |> Frame.loadUrl
          ]
          |> T.batch
        Nothing -> T.none
      )

contents : Model.Frame -> List (Html Msg)
contents model =
  [ H.section [ A.class "edit" ]
    [ model |> content
    ]
  ]

content : Model.Frame -> Html Msg
content model = L.lazy
  (\register -> Html.register
    { view   = register.form |> View.view
    , upload = register.upload
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
      { upload = Upload
      , input  = Input
      , toggle = Toggle
      , change = Change
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
