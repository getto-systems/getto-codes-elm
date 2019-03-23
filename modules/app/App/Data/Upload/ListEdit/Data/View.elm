module GettoCodes.App.Data.Upload.ListEdit.Data.View exposing
  ( Response
  , Upload
  , response
  , upload
  , http
  , toResponse
  , isDifferentResponse
  )
import GettoCodes.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { etag : String
  }
type alias ResponseBody = Upload
type alias Upload =
  { id     : Int
  , name   : String
  , gender : String
  }

response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.map ResponseHeader
    ( HeaderDecode.at "etag" HeaderDecode.string )
  , body = upload
  }

upload : Decode.Decoder Upload
upload = Decode.map3 Upload
  ( Decode.at ["id"]     Decode.int )
  ( Decode.at ["name"]   Decode.string )
  ( Decode.at ["gender"] Decode.string )

http : ResponseBody -> HttpView.Model Response
http row = HttpView.empty |> HttpView.update (HttpView.success (row |> toResponse))

toResponse : ResponseBody -> Response
toResponse = HttpView.toResponse { etag = "" }

isDifferentResponse : Response -> Response -> Bool
isDifferentResponse res last = ( res |> etag ) /= ( last |> etag )

etag : Response -> String
etag = HttpView.header >> .etag
