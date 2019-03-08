module GettoUpload.App.Upload.Edit.Info exposing
  ( Model
  , Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  )
import GettoUpload.App.Upload.Edit.Model as Model
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

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Validate as Validate
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | data : Model.Data, info : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { form : View.Form
  , put  : HttpView.Model View.Response
  }

type Msg
  = Edit
  | Static
  | Change
  | Request
  | Input   (View.Prop String) String
  | Resolve (View.Prop String) (Conflict.Resolve String)
  | StateChanged (HttpView.Migration View.Response)

signature = "info"

put : Http.Tracker (FrameModel a) View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .info
    in
      Http.put
        { url      = "upload/:id/info" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = m.form |> View.params data.get
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put StateChanged
putRequest = Http.request signature put StateChanged


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Model.Data -> Model -> Encode.Value
encodeStore data model = Encode.object
  [ ( data.id |> String.fromInt
    , model.form |> View.encodeForm
    )
  ]

decodeStore : Model.Data -> Decode.Value -> Model -> Model
decodeStore data value model =
  { model
  | form = model.form |> View.decodeForm
    (value |> SafeDecode.valueAt [data.id |> String.fromInt])
  }

subscriptions : Model -> Sub Msg
subscriptions model = putTrack

update : Model.Data -> Msg -> Model -> ( Model, ( FrameTransition a, Bool ) )
update data msg model =
  case msg of
    Edit    -> ( { model | form = model.form |> Edit.toEdit View.edit data.get }, ( fillAndStore,   False ) )
    Static  -> ( { model | form = model.form |> Edit.toStatic },                  ( Frame.storeApp, False ) )
    Change  -> ( { model | form = model.form |> Edit.change },                    ( Frame.storeApp, False ) )
    Request -> ( { model | form = model.form |> Edit.commit },                    ( putRequest,     False ) )

    Input   prop value -> ( { model | form = model.form |> Form.set prop value },       ( T.none,       False ) )
    Resolve prop mig   -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )

    StateChanged mig ->
      ( { model
        | put  = model.put  |> HttpView.update mig
        , form = model.form |> Edit.put mig
        }
      , ( T.none
        , mig |> HttpView.isComplete
        )
      )

fill : FrameTransition a
fill = Frame.app >> .info >> .form >> View.pairs >> Dom.fill

fillAndStore : FrameTransition a
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> info
  ]

info : FrameModel a -> Html Msg
info model = L.lazy2
  (\data m -> Html.info
    { view = m.form |> View.view data.get
    , put  = m.put  |> HttpView.state
    , msg =
      { put     = Request
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
