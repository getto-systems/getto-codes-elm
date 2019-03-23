module GettoCodes.App.Data.Upload.Edit.Complete exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoCodes.App.Data.Upload.Edit.Model as Model
import GettoCodes.App.Data.Upload.Edit.Complete.View as View
import GettoCodes.App.Data.Upload.Edit.Complete.Html as Html
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Api as Api
import GettoCodes.Command.Http as Http
import GettoCodes.View.Http as HttpView
import GettoCodes.I18n.App as AppI18n
import GettoCodes.I18n.App.Data.Upload as I18n
import GettoCodes.I18n.Http as HttpI18n

import Getto.Command.Transition as T exposing ( Transition )

import Json.Encode as Encode
import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Lazy as L

type Msg
  = Edit
  | Static
  | Complete
  | CompleteStateChanged (HttpView.Migration View.Response)
  | Work
  | WorkStateChanged     (HttpView.Migration View.Response)

signature = "complete"

putComplete : Http.Tracker Model.Frame View.Response
putComplete = Http.tracker "putComplete" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      complete = model |> Frame.app |> .complete
    in
      Http.putIfMatch ( data |> Model.etag )
        { url      = "upload/:id/complete" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

putCompleteTrack   = Http.track   signature putComplete CompleteStateChanged
putCompleteRequest = Http.request signature putComplete CompleteStateChanged

putWork : Http.Tracker Model.Frame View.Response
putWork = Http.tracker "putWork" <|
  \model ->
    let
      data     = model |> Frame.app |> .data
      complete = model |> Frame.app |> .complete
    in
      Http.putIfMatch ( data |> Model.etag )
        { url      = "upload/:id/work" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

putWorkTrack   = Http.track   signature putWork WorkStateChanged
putWorkRequest = Http.request signature putWork WorkStateChanged


init : Frame.InitModel -> ( Model.Complete, Model.Transition Msg )
init model =
  ( { isEdit      = False
    , putComplete = HttpView.empty
    , putWork     = HttpView.empty
    }
  , T.none
  )

encodeStore : Model.Complete -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model.Complete -> Model.Complete
decodeStore value model = model

subscriptions : Model.Complete -> Sub Msg
subscriptions model =
  [ putCompleteTrack
  , putWorkTrack
  ] |> Sub.batch

update : Msg -> Model.Complete -> ( Model.Complete, ( Model.Transition Msg, Bool ) )
update msg model =
  case msg of
    Edit   -> ( { model | isEdit = True  }, ( T.none, False ) )
    Static -> ( { model | isEdit = False }, ( T.none, False ) )

    Complete -> ( model, ( putCompleteRequest, False ) )
    CompleteStateChanged mig ->
      ( { model
        | isEdit      = model.isEdit      |> toStaticWhenSuccess mig
        , putComplete = model.putComplete |> HttpView.update mig
        }
      , ( T.none, mig |> HttpView.isComplete )
      )

    Work -> ( model, ( putWorkRequest, False ) )
    WorkStateChanged mig ->
      ( { model
        | isEdit  = model.isEdit  |> toStaticWhenSuccess mig
        , putWork = model.putWork |> HttpView.update mig
        }
      , ( T.none, mig |> HttpView.isComplete )
      )

toStaticWhenSuccess : HttpView.Migration response -> Bool -> Bool
toStaticWhenSuccess mig =
  case mig |> HttpView.isSuccess of
    Nothing -> identity
    Just _  -> always False


contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> state
  ]

state : Model.Frame -> Html Msg
state model = L.lazy
  (\data -> Html.state
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

dialogs : Model.Frame -> List (Html Msg)
dialogs model =
  [ model |> dialog
  ]

dialog : Model.Frame -> Html Msg
dialog model = L.lazy2
  (\data complete -> Html.dialog
    { get      = data.get
    , isEdit   = complete.isEdit
    , complete = complete.putComplete |> HttpView.state
    , work     = complete.putWork     |> HttpView.state
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
