module GettoCodes.Layout.Page.Options exposing
  ( Msg
  , init
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  )
import GettoCodes.Layout.Page.Model as Model
import GettoCodes.Layout.Page.Options.View as View
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Api as Api
import GettoCodes.Command.Http as Http
import GettoCodes.View.Http as HttpView

import Getto.Url.Query.Encode as QueryEncode

import Json.Encode as Encode
import Json.Decode as Decode

type Msg
  = StateChanged (HttpView.Migration View.Response)

signature = "layout-options"

get : Http.Tracker (Model.Frame app) View.Response
get = Http.tracker "get" <|
  \model ->
    let
      options = model |> Frame.layout |> .options
    in
      Http.getIfNoneMatch ( options.get |> etag )
        { url      = "layout/options" |> Api.url []
        , headers  = model |> Api.headers
        , params   = QueryEncode.null
        , response = View.response
        , timeout  = 10 * 1000
        }

etag : HttpView.Model View.Response -> Maybe String
etag = HttpView.response >> Maybe.map (HttpView.header >> .etag )

getTrack   = Http.track   signature get StateChanged
getRequest = Http.request signature get StateChanged


init : Frame.InitModel -> ( Model.Options, Model.Transition app Msg )
init model =
  ( { get = HttpView.empty
    }
  , getRequest
  )

encodeStore : Model.Options -> Encode.Value
encodeStore model =
  model.get
  |> HttpView.response
  |> Maybe.map View.encodeResponse
  |> Maybe.withDefault Encode.null

decodeStore : Decode.Value -> Model.Options -> Model.Options
decodeStore value model =
  { model
  | get = model.get |>
    case value |> Decode.decodeValue View.decodeResponse of
      Ok res -> res |> HttpView.success |> HttpView.update
      Err _  -> identity
  }

subscriptions : Model.Options -> Sub Msg
subscriptions model = getTrack

update : Msg -> Model.Options -> ( Model.Options, Model.Transition app Msg )
update msg model =
  case msg of
    StateChanged mig -> ( { model | get = model.get |> HttpView.update mig }, Frame.storeLayout )
