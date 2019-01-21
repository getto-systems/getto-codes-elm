module GettoUpload.Layout.Href exposing
  ( prependRoot
  , withQuery
  )
import GettoUpload.Env.App as Env

import Getto.Url.Query.Encode as Encode

prependRoot : String -> String
prependRoot path = Env.root ++ path

withQuery : Encode.Value -> String -> String
withQuery params path = path ++ (params |> Encode.encode)
