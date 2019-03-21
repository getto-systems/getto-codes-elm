module GettoUpload.App.Data.Upload.Edit.Info exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  )
import GettoUpload.App.Data.Upload.Edit.Model as Model
import GettoUpload.App.Data.Upload.Edit.Data.View as Data
import GettoUpload.App.Data.Upload.Edit.Info.View as View
import GettoUpload.App.Data.Upload.Edit.Info.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.Command.Dom as Dom
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Data.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode
import Getto.Field.Form as Form
import Getto.Field.Edit as Edit
import Getto.Field.Conflict as Conflict

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Edit
  | Cancel
  | Change
  | Request
  | Input   (View.Prop String) String
  | Resolve (View.Prop String) (Conflict.Resolve String)
  | StateChanged (HttpView.Migration View.Response)

signature = "info"

put : Http.Tracker Model.Frame View.Response
put = Http.tracker "put" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      info = model |> Frame.app |> .info
    in
      Http.put
        { url      = "upload/:id/info" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = info.form |> View.params data.get
        , response = View.response
        , timeout  = 10 * 1000
        }

putTrack   = Http.track   signature put StateChanged
putRequest = Http.request signature put StateChanged


init : Frame.InitModel -> ( Model.Info, Model.Transition Msg )
init model =
  ( { form = signature |> View.init
    , put  = HttpView.empty
    }
  , fill
  )

encodeStore : Model.Info -> Encode.Value
encodeStore model = model.form |> View.encodeForm

decodeStore : Decode.Value -> Model.Info -> Model.Info
decodeStore value model = { model | form = model.form |> View.decodeForm value }

subscriptions : Model.Info -> Sub Msg
subscriptions model = putTrack

update : Model.Data -> Msg -> Model.Info -> ( Model.Info, ( Model.Transition Msg, Bool ) )
update data msg model =
  case msg of
    Edit    -> ( { model | form = model.form |> edit data.get }, ( fillAndStore,   False ) )
    Cancel  -> ( { model | form = model.form |> Edit.cancel },   ( Frame.storeApp, False ) )
    Change  -> ( { model | form = model.form |> Edit.change },   ( Frame.storeApp, False ) )
    Request -> ( { model | form = model.form |> Edit.commit },   ( putRequest,     False ) )

    Input   prop value -> ( { model | form = model.form |> Form.set prop value },       ( T.none,       False ) )
    Resolve prop mig   -> ( { model | form = model.form |> Conflict.resolve prop mig }, ( fillAndStore, False ) )

    StateChanged mig ->
      ( { model | put = model.put |> HttpView.update mig }
      , ( T.none, mig |> HttpView.isComplete )
      )

edit : HttpView.Model Data.Response -> View.Form -> View.Form
edit = HttpView.response >> Edit.edit View.edit

fill : Model.Transition Msg
fill = Frame.app >> .info >> .form >> Edit.fields >> Html.pairs >> Dom.fill

fillAndStore : Model.Transition Msg
fillAndStore = [ fill, Frame.storeApp ] |> T.batch


contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> content
  ]

content : Model.Frame -> Html Msg
content model = L.lazy2
  (\data info -> Html.info
    { view = View.view info.put info.form data.get
    , get  = data.get
    , put  = info.put
    , msg =
      { put     = Request
      , input   = Input
      , resolve = Resolve
      , change  = Change
      , edit    = Edit
      , cancel  = Cancel
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
