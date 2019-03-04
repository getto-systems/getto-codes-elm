module GettoUpload.App.Upload.Edit.Info exposing
  ( Model
  , Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.Edit.Data as Data
import GettoUpload.App.Upload.Edit.Info.View as View
import GettoUpload.App.Upload.Edit.Info.Html as Html
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

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Url.Query.SafeDecode as QuerySafeDecode
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode
import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | data : Data.Model, info : Model }
type alias DataTransition a = Transition (FrameModel a) Data.Msg
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form : View.Form
  , put  : HttpView.Model View.Response
  }

type Msg
  = Edit
  | Static
  | Input   (View.Prop String) String
  | Resolve (View.Prop String) (Conflict.Resolve String)
  | Change
  | PutRequest
  | PutStateChanged (HttpView.Migration View.Response)

signature = "info"

put : Http.Tracker (FrameModel a) View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m = model |> Frame.app |> .info
    in
      Http.put ( data |> Data.etag )
        { url     = "upload/:id/info" |> Api.url ( data |> Data.pathInfo )
        , headers = model |> Api.headers
        , params  = Encode.object
          [ ( "name",  m.form.name  |> Field.value |> Encode.string )
          , ( "memo",  m.form.memo  |> Field.value |> Encode.string )
          , ( "age",   m.form.age   |> Field.value |> Encode.string )
          , ( "email", m.form.email |> Field.value |> Encode.string )
          , ( "tel",   m.form.tel   |> Field.value |> Encode.string )
          ]
        , response = response
        , timeout  = 10 * 1000
        }

response : HttpView.ResponseDecoder View.Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Data.Model -> Model -> Encode.Value
encodeStore data model = Encode.object
  [ ( data.id |> String.fromInt
    , model.form |> View.encodeForm
    )
  ]

decodeStore : Data.Model -> Decode.Value -> Model -> Model
decodeStore data value model =
  { model
  | form = model.form |> View.decodeForm
    (value |> SafeDecode.valueAt [data.id |> String.fromInt])
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track signature put PutStateChanged

update : Data.Model -> Msg -> Model -> ( Model, ( DataTransition a, FrameTransition a ) )
update data msg model =
  case msg of
    Edit   -> ( { model | form = model.form |> edit data },     ( T.none, fillAndStore ) )
    Static -> ( { model | form = model.form |> View.toStatic }, ( T.none, T.none ) )

    Input   prop value -> ( { model | form = model.form |> Form.set prop value },       ( T.none, T.none ) )
    Resolve prop mig   -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( T.none, fillAndStore ) )

    Change -> ( { model | form = model.form |> View.changed }, ( T.none, Frame.storeApp ) )

    PutRequest ->
      ( { model | form = model.form |> View.toCommit }
      , ( T.none
        , Http.request signature put PutStateChanged
        )
      )
    PutStateChanged mig ->
      ( { model | put = model.put  |> HttpView.update mig }
      , ( case mig |> HttpView.isSuccess of
          Just _  -> Data.request
          Nothing -> T.none
        , T.none
        )
      )

edit : Data.Model -> View.Form -> View.Form
edit data form =
  case data.get |> HttpView.response of
    Nothing -> form
    Just res -> form |> View.toEdit res

fill : FrameTransition a
fill = Frame.app >> .info >>
  (\model -> Dom.fill
    [ model.form.name     |> Dom.string
    , model.form.memo     |> Dom.string
    , model.form.age      |> Dom.string
    , model.form.email    |> Dom.string
    , model.form.tel      |> Dom.string
    ]
  )

fillAndStore : FrameTransition a
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> info
  ]

info : FrameModel a -> Html Msg
info model = L.lazy2
  (\data m -> Html.info
    { title = signature
    , view = m.form |> View.view data.get
    , put  = m.put  |> HttpView.state
    , msg =
      { put     = PutRequest
      , input   = Input
      , resolve = Resolve
      , change  = Change
      , edit    = Edit
      , static  = Static
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
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .info)

dialogs : FrameModel a -> List (Html Msg)
dialogs model = []
