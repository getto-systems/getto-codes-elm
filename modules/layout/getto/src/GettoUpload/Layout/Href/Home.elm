module GettoUpload.Layout.Href.Home exposing
  ( index
  )
import GettoUpload.Layout.Href as Href

import Getto.Url.Query.Encode as QueryEncode

index = "index.html" |> Href.href QueryEncode.empty
