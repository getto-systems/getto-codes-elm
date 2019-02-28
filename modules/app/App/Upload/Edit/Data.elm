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
  , encodeResponse
  , decodeStore
  , decodeResponse
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
import Getto.Http.Header.Decode as HeaderDecode
import Getto.Json.SafeDecode as SafeDecode

import Set exposing ( Set )
import Json.Encode as Encode
import Json.Decode as Decode
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
        , response = response
        , timeout = 10 * 1000
        }

response : HttpView.ResponseDecoder View.Response
response =  HttpView.decoder
  { header = HeaderDecode.map View.ResponseHeader
    ( HeaderDecode.at "etag" HeaderDecode.string )
  , body = Decode.map2 View.ResponseBody
    ( Decode.at ["info"]
      ( Decode.map5 View.ResponseInfo
        (Decode.at ["name"]  Decode.string)
        (Decode.at ["memo"]  Decode.string)
        (Decode.at ["age"]   Decode.int)
        (Decode.at ["email"] Decode.string)
        (Decode.at ["tel"]   Decode.string)
      )
    )
    ( Decode.at ["detail"]
      ( Decode.map5 View.ResponseDetail
        (Decode.at ["birthday"] Decode.string)
        (Decode.at ["start_at"] Decode.string)
        (Decode.at ["gender"]   Decode.string)
        (Decode.at ["quality"]  Decode.string)
        (Decode.at ["roles"]   (Decode.string |> Decode.list |> Decode.map Set.fromList))
      )
    )
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
    , [ ( "response", model.get |> HttpView.response |> Maybe.map encodeResponse |> Maybe.withDefault Encode.null )
      ] |> Encode.object
    )
  ]

encodeResponse : View.Response -> Encode.Value
encodeResponse res =
  let
    header = res |> HttpView.header
    body   = res |> HttpView.body
  in
    [ ( "header"
      , [ ( "etag", header.etag |> Encode.string )
        ] |> Encode.object
      )
    , ( "body"
      , [ ( "info"
          , [ ( "name",  body.info.name  |> Encode.string )
            , ( "memo",  body.info.memo  |> Encode.string )
            , ( "age",   body.info.age   |> Encode.int )
            , ( "email", body.info.email |> Encode.string )
            , ( "tel",   body.info.tel   |> Encode.string )
            ] |> Encode.object
          )
        , ( "detail"
          , [ ( "birthday", body.detail.birthday |> Encode.string )
            , ( "start_at", body.detail.start_at |> Encode.string )
            , ( "gender",   body.detail.gender   |> Encode.string )
            , ( "quality",  body.detail.quality  |> Encode.string )
            , ( "roles",    body.detail.roles |> Set.toList |> Encode.list Encode.string )
            ] |> Encode.object
          )
        ] |> Encode.object
      )
    ] |> Encode.object

decodeStore : Decode.Value -> Model -> Model
decodeStore value model =
  let
    obj = value |> SafeDecode.valueAt [model.id |> String.fromInt]
  in
    { model
    | get = model.get |>
      case obj |> Decode.decodeValue (Decode.at ["response"] decodeResponse) of
        Ok res -> res |> HttpView.success |> HttpView.update
        Err _  -> identity
    }

decodeResponse : Decode.Decoder View.Response
decodeResponse =
  ( Decode.map2 HttpView.ResponseValue
    ( Decode.at ["header"] (Decode.dict Decode.string) )
    ( Decode.at ["body"]   Decode.value )
  )
  |> Decode.andThen
    (\value ->
      case value |> response of
        Ok  res -> res |> Decode.succeed
        Err err -> err |> Decode.fail
    )

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
