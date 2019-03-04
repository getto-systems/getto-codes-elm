module GettoUpload.App.Upload.Edit.Data exposing
  ( Model
  , Msg
  , etag
  , pathInfo
  , request
  , init
  , encodeQuery
  , decodeQuery
  , encodeStore
  , decodeStore
  , subscriptions
  , update
  )
import GettoUpload.App.Upload.Edit.Data.View as View
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Api as Api
import GettoUpload.Command.Http as Http
import GettoUpload.View.Http as HttpView

import Getto.Command.Transition as Transition exposing ( Transition )
import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode
import Getto.Url.Query.SafeDecode as QuerySafeDecode
import Getto.Json.SafeDecode as SafeDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )
import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Lazy as L

type alias FrameModel a = Frame.Model Layout.Model { a | data : Model }
type alias FrameTransition a = Transition (FrameModel a) Msg
type alias Model =
  { id  : Int
  , get : HttpView.Model View.Response
  }

type Msg
  = GetStateChanged (HttpView.Migration View.Response)

signature = "data"

get : Http.Tracker (FrameModel a) View.Response
get = Http.tracker "get" <|
  \model ->
    let
      m = model |> Frame.app |> .data
    in
      Http.getIfNoneMatch ( m |> etag )
        { url      = "upload/:id" |> Api.url ( m |> pathInfo )
        , headers  = model |> Api.headers
        , params   = QueryEncode.empty
        , response = View.response
        , timeout = 10 * 1000
        }

etag : Model -> Maybe String
etag model = model.get |> HttpView.response |> Maybe.map (HttpView.header >> .etag)

pathInfo : Model -> List ( String, String )
pathInfo model =
  [ ( "id", model.id |> String.fromInt )
  ]

request : FrameTransition a
request = Http.request signature get GetStateChanged


init : Frame.InitModel -> ( Model, FrameTransition a )
init model =
  ( { id  = 0
    , get = HttpView.empty
    }
  , [ request
    ] |> Transition.batch
  )

encodeQuery : Model -> QueryEncode.Value
encodeQuery model = QueryEncode.empty

decodeQuery : List String -> QueryDecode.Value -> Model -> Model
decodeQuery names value model =
  let
    entryAt name = QuerySafeDecode.entryAt (names ++ [name])
  in
    { model | id = value |> entryAt "id" (QuerySafeDecode.int 0) }

encodeStore : Model -> Encode.Value
encodeStore model = Encode.object
  [ ( model.id |> String.fromInt
    , [ ( "response"
        , model.get |> HttpView.response |> Maybe.map View.encodeResponse |> Maybe.withDefault Encode.null
        )
      ] |> Encode.object
    )
  ]

decodeStore : Decode.Value -> Model -> Model
decodeStore value model =
  let
    obj = value |> SafeDecode.valueAt [model.id |> String.fromInt]
  in
    { model
    | get = model.get |>
      case obj |> Decode.decodeValue (Decode.at ["response"] View.decodeResponse) of
        Ok res -> res |> HttpView.success |> HttpView.update
        Err _  -> identity
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Http.track signature get GetStateChanged

update : Msg -> Model -> ( Model, FrameTransition a )
update msg model =
  case msg of
    GetStateChanged mig ->
      ( { model | get = model.get |> HttpView.update mig }
      , case mig |> HttpView.isSuccess of
        Just _  -> Frame.storeApp
        Nothing -> Transition.none
      )
