module GettoUpload.Layout.Page.Options.View exposing
  ( Response
  , response
  , encodeResponse
  , decodeResponse
  , role
  , gender
  , quality
  )
import GettoUpload.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Encode as Encode
import Json.Decode as Decode

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { etag : String
  }
type alias ResponseBody =
  { role    : List String
  , gender  : List String
  , quality : List String
  }


response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "etag" HeaderDecode.string )
  , body = Decode.map3 ResponseBody
    ( Decode.at ["role"]    (Decode.list Decode.string) )
    ( Decode.at ["gender"]  (Decode.list Decode.string) )
    ( Decode.at ["quality"] (Decode.list Decode.string) )
  }

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
      , [ ( "role",    body.role    |> Encode.list Encode.string )
        , ( "gender",  body.gender  |> Encode.list Encode.string )
        , ( "quality", body.quality |> Encode.list Encode.string )
        ] |> Encode.object
      )
    ] |> Encode.object

decodeResponse : Decode.Decoder Response
decodeResponse = HttpView.decodeResponse response


role : HttpView.Model Response -> Maybe (List String)
role = responseBody .role

gender : HttpView.Model Response -> Maybe (List String)
gender = responseBody .gender

quality : HttpView.Model Response -> Maybe (List String)
quality = responseBody .quality

responseBody : (ResponseBody -> a) -> HttpView.Model Response -> Maybe a
responseBody getter = HttpView.response >> Maybe.map (HttpView.body >> getter)
