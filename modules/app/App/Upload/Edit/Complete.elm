module GettoUpload.App.Upload.Edit.Complete exposing
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
import GettoUpload.App.Upload.Edit.Model as Model
import GettoUpload.App.Upload.Edit.Complete.View as View
import GettoUpload.App.Upload.Edit.Complete.Html as Html
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | data : Model.Data, complete : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { isEdit   : Bool
  , complete : HttpView.Model View.Response
  , work     : HttpView.Model View.Response
  }

type Msg
  = Edit
  | Static
  | Complete
  | CompleteStateChanged (HttpView.Migration View.Response)
  | Work
  | WorkStateChanged     (HttpView.Migration View.Response)

signature = "complete"

complete : Http.Tracker (FrameModel a) View.Response
complete = Http.tracker "complete" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .complete
    in
      Http.putIfMatch ( data |> Model.etag )
        { url      = "upload/:id/complete" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

completeTrack   = Http.track   signature complete CompleteStateChanged
completeRequest = Http.request signature complete CompleteStateChanged

work : Http.Tracker (FrameModel a) View.Response
work = Http.tracker "work" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .complete
    in
      Http.putIfMatch ( data |> Model.etag )
        { url      = "upload/:id/work" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

workTrack   = Http.track   signature work WorkStateChanged
workRequest = Http.request signature work WorkStateChanged


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { isEdit   = False
    , complete = HttpView.empty
    , work     = HttpView.empty
    }
  , T.none
  )

encodeStore : Model -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model -> Model
decodeStore value model = model

subscriptions : Model -> Sub Msg
subscriptions model =
  [ completeTrack
  , workTrack
  ] |> Sub.batch

update : Msg -> Model -> ( Model, ( FrameTransition a, Bool ) )
update msg model =
  case msg of
    Edit   -> ( { model | isEdit = True  }, ( T.none, False ) )
    Static -> ( { model | isEdit = False }, ( T.none, False ) )

    Complete -> ( model, ( completeRequest, False ) )
    CompleteStateChanged mig ->
      ( { model
        | isEdit   = model.isEdit   |> toStaticWhenSuccess mig
        , complete = model.complete |> HttpView.update mig
        }
      , ( T.none, mig |> HttpView.isComplete )
      )

    Work -> ( model, ( workRequest, False ) )
    WorkStateChanged mig ->
      ( { model
        | isEdit = model.isEdit |> toStaticWhenSuccess mig
        , work   = model.work   |> HttpView.update mig
        }
      , ( T.none, mig |> HttpView.isComplete )
      )

toStaticWhenSuccess : HttpView.Migration response -> Bool -> Bool
toStaticWhenSuccess mig =
  case mig |> HttpView.isSuccess of
    Nothing -> identity
    Just _  -> always False


contents : FrameModel a -> List (Html Msg)
contents model =
  [ model |> state
  ]

state : FrameModel a -> Html Msg
state model = L.lazy2
  (\data m -> Html.state
    { get = data.get
    , msg =
      { edit = Edit
      }
    , i18n =
      { title = I18n.title
      , field = I18n.field
      , state = I18n.state
      , form  = AppI18n.form
      }
    }
  )
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .complete)

dialogs : FrameModel a -> List (Html Msg)
dialogs model =
  [ model |> dialog
  ]

dialog : FrameModel a -> Html Msg
dialog model = L.lazy2
  (\data m -> Html.dialog
    { get      = data.get
    , isEdit   = m.isEdit
    , complete = m.complete |> HttpView.state
    , work     = m.work     |> HttpView.state
    , msg =
      { complete = Complete
      , work     = Work
      , cancel   = Static
      }
    , i18n =
      { title   = I18n.title
      , message = I18n.message
      , button  = I18n.button
      , form    = AppI18n.form
      , http    = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .complete)
