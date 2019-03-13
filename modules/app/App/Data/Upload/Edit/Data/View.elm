module GettoUpload.App.Data.Upload.Edit.Data.View exposing
  ( Response
  , State(..)
  , response
  , encodeResponse
  , decodeResponse
  , isDifferentResponse
  )
import GettoUpload.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

import Set exposing ( Set )

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { etag : String
  }
type alias ResponseBody =
  { info   : ResponseInfo
  , detail : ResponseDetail
  , state  : ResponseState
  }
type alias ResponseInfo =
  { name     : String
  , memo     : String
  , age      : Int
  , email    : String
  , tel      : String
  }
type alias ResponseDetail =
  { birthday : String
  , start_at : String
  , gender   : String
  , quality  : String
  , roles    : Set String
  }
type alias ResponseState =
  { state : State
  }

type State
  = Working
  | Complete

response : HttpView.ResponseDecoder Response
response =  HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "etag" HeaderDecode.string )
  , body = Decode.map3 ResponseBody
    ( Decode.at ["info"]
      ( Decode.map5 ResponseInfo
        (Decode.at ["name"]  Decode.string)
        (Decode.at ["memo"]  Decode.string)
        (Decode.at ["age"]   Decode.int)
        (Decode.at ["email"] Decode.string)
        (Decode.at ["tel"]   Decode.string)
      )
    )
    ( Decode.at ["detail"]
      ( Decode.map5 ResponseDetail
        (Decode.at ["birthday"] Decode.string)
        (Decode.at ["start_at"] Decode.string)
        (Decode.at ["gender"]   Decode.string)
        (Decode.at ["quality"]  Decode.string)
        (Decode.at ["roles"]   (Decode.string |> Decode.list |> Decode.map Set.fromList))
      )
    )
    ( Decode.at ["state"]
      ( Decode.map ResponseState
        (Decode.at ["state"] (Decode.string |> Decode.map toState))
      )
    )
  }

toState : String -> State
toState state =
  case state of
    "complete" -> Complete
    _          -> Working

fromState : State -> String
fromState state =
  case state of
    Complete -> "complete"
    Working  -> "working"

encodeResponse : Response -> Encode.Value
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
        , ( "state"
          , [ ( "state", body.state.state |> fromState |> Encode.string )
            ] |> Encode.object
          )
        ] |> Encode.object
      )
    ] |> Encode.object

decodeResponse : Decode.Decoder Response
decodeResponse = HttpView.decodeResponse response

isDifferentResponse : Response -> Response -> Bool
isDifferentResponse res last = ( res |> etag ) /= ( last |> etag )

etag : Response -> String
etag = HttpView.header >> .etag
