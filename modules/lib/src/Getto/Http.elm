module Getto.Http exposing
  ( Request
  , Response
  , multipartBody
  )
import Getto.Http.Part as Part

import Json.Encode as Encode
import Json.Decode as Decode
import Http

type alias Request =
  { method  : String
  , headers : List Http.Header
  , url     : String
  , body    : Http.Body
  , timeout : Maybe Float
  , tracker : Maybe String
  }

type alias Response = Result Http.Error Decode.Value

multipartBody : Part.Model -> Http.Body
multipartBody = Part.toBody
