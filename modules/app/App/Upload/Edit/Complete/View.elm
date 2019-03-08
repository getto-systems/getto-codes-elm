module GettoUpload.App.Upload.Edit.Complete.View exposing
  ( Response
  , response
  )
import GettoUpload.View.Http as HttpView

import Getto.Http.Header.Decode as HeaderDecode

import Json.Decode as Decode

type alias Response = HttpView.Response () ()

response : HttpView.ResponseDecoder Response
response = HttpView.decoder
  { header = HeaderDecode.succeed ()
  , body   = Decode.succeed ()
  }
