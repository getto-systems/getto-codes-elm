module GettoUpload.Extension.Href.Upload exposing
  ( list
  )
import GettoUpload.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "upload/" ++ path |> Href.internal params

list = "list.html" |> href QueryEncode.empty
