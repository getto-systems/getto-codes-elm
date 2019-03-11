module GettoUpload.App.Upload.Edit.Unregister exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  , contents
  , dialogs
  )
import GettoUpload.App.Upload.Edit.Model as Model
import GettoUpload.App.Upload.Edit.Unregister.View as View
import GettoUpload.App.Upload.Edit.Unregister.Html as Html
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.View.Http as HttpView
import GettoUpload.I18n.App as AppI18n
import GettoUpload.I18n.App.Upload as I18n
import GettoUpload.I18n.Http as HttpI18n
import GettoUpload.Extension.Href as Href
import GettoUpload.Extension.Href.Upload as Upload

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Browser.Navigation as Navigation
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type Msg
  = Confirm
  | Cancel
  | Unregister
  | StateChanged (HttpView.Migration View.Response)

signature = "unregister"

unregister : Http.Tracker Model.Frame View.Response
unregister = Http.tracker "unregister" <|
  \model ->
    let
      data = model |> Frame.app |> .data
      m    = model |> Frame.app |> .unregister
    in
      Http.delete ( data |> Model.etag )
        { url      = "upload/:id" |> Api.url ( data |> Model.pathInfo )
        , headers  = model  |> Api.headers
        , params   = Encode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

unregisterTrack   = Http.track   signature unregister StateChanged
unregisterRequest = Http.request signature unregister StateChanged


init : Frame.InitModel -> ( Model.Unregister, Model.Transition Msg )
init model =
  ( { isConfirm  = False
    , unregister = HttpView.empty
    }
  , T.none
  )

encodeStore : Model.Unregister -> Encode.Value
encodeStore model = Encode.null

decodeStore : Decode.Value -> Model.Unregister -> Model.Unregister
decodeStore value model = model

subscriptions : Model.Unregister -> Sub Msg
subscriptions model = unregisterTrack

update : Msg -> Model.Unregister -> ( Model.Unregister, Model.Transition Msg )
update msg model =
  case msg of
    Confirm -> ( { model | isConfirm = True  }, T.none )
    Cancel  -> ( { model | isConfirm = False }, T.none )

    Unregister -> ( model, unregisterRequest )
    StateChanged mig ->
      ( { model | unregister = model.unregister |> HttpView.update mig }
      , case mig |> HttpView.isSuccess of
        Nothing -> T.none
        Just _  -> always ( Upload.list |> Href.toString |> Navigation.load )
      )


contents : Model.Frame -> List (Html Msg)
contents model =
  [ model |> delete
  ]

delete : Model.Frame -> Html Msg
delete model = L.lazy2
  (\data m -> Html.unregister
    { get = data.get
    , msg =
      { confirm = Confirm
      }
    , i18n =
      { title = I18n.title
      , form  = AppI18n.form
      }
    }
  )
  (model |> Frame.app |> .data)
  (model |> Frame.app |> .unregister)

dialogs : Model.Frame -> List (Html Msg)
dialogs model =
  [ model |> dialog
  ]

dialog : Model.Frame -> Html Msg
dialog model = L.lazy
  (\m -> Html.dialog
    { isConfirm  = m.isConfirm
    , unregister = m.unregister |> HttpView.state
    , msg =
      { unregister = Unregister
      , cancel     = Cancel
      }
    , i18n =
      { title   = I18n.title
      , message = I18n.message
      , form    = AppI18n.form
      , http    = HttpI18n.error
      }
    }
  )
  (model |> Frame.app |> .unregister)
