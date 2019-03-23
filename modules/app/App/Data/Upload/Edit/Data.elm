module GettoCodes.App.Data.Upload.Edit.Data exposing
  ( Msg
  , getRequest
  , init
  , encodeQuery
  , decodeQuery
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  )
import GettoCodes.App.Data.Upload.Edit.Model as Model
import GettoCodes.App.Data.Upload.Edit.Data.View as View
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Api as Api
import GettoCodes.Command.Http as Http
import GettoCodes.View.Http as HttpView

import Getto.Command.Transition as T exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Url.Query.SafeDecode as QuerySafeDecode
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

type Msg
  = StateChanged (HttpView.Migration View.Response)

signature = "data"

get : Http.Tracker Model.Frame View.Response
get = Http.tracker "get" <|
  \model ->
    let
      data = model |> Frame.app |> .data
    in
      Http.getIfNoneMatch ( data |> Model.etag )
        { url      = "upload/:id" |> Api.url ( data |> Model.pathInfo )
        , headers  = model |> Api.headers
        , params   = QueryEncode.null
        , response = View.response
        , timeout = 10 * 1000
        }

getTrack   = Http.track   signature get StateChanged
getRequest = Http.request signature get StateChanged


init : Frame.InitModel -> ( Model.Data, Model.Transition Msg )
init model =
  ( { id  = 0
    , get = HttpView.empty
    }
  , getRequest
  )

encodeQuery : Model.Data -> QueryEncode.Value
encodeQuery model = QueryEncode.null

decodeQuery : List String -> QueryDecode.Value -> Model.Data -> Model.Data
decodeQuery names value model =
  let
    entryAt name = QuerySafeDecode.entryAt (names ++ [name])
  in
    { model | id = value |> entryAt "id" (QuerySafeDecode.int 0) }

encodeStore : Model.Data -> Encode.Value
encodeStore model =
  model.get
  |> HttpView.response
  |> Maybe.map View.encodeResponse
  |> Maybe.withDefault Encode.null

decodeStore : Decode.Value -> Model.Data -> Model.Data
decodeStore value model =
  { model
  | get = model.get |>
    case value |> Decode.decodeValue View.decodeResponse of
      Ok res -> res |> HttpView.success |> HttpView.update
      Err _  -> identity
  }

subscriptions : Model.Data -> Sub Msg
subscriptions model = getTrack

update : Msg -> Model.Data -> ( Model.Data, Model.Transition Msg )
update msg model =
  case msg of
    StateChanged mig ->
      ( { model | get = model.get |> HttpView.update mig }
      , if mig |> HttpView.isComplete
        then Frame.storeApp
        else T.none
      )
