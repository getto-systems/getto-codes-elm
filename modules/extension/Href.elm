module GettoUpload.Extension.Href exposing
  ( Href
  , path
  , href
  , toString
  )
import GettoUpload.Env.App as Env

import Getto.Url.Query.Encode as QueryEncode

type Href = Href
  { path  : String
  , query : QueryEncode.Value
  }

path : Href -> String
path (Href data) = data.path

href : QueryEncode.Value -> String -> Href
href query pathData =
  Href
    { path  = pathData
    , query = query
    }

toString : Href -> String
toString (Href data) = data.path |> prependRoot |> withQuery data.query

prependRoot : String -> String
prependRoot pathData = Env.hrefRoot ++ pathData

withQuery : QueryEncode.Value -> String -> String
withQuery query pathData = pathData ++ (query |> QueryEncode.encode)
