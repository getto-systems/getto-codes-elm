module GettoUpload.Extension.Href.Home exposing
  ( index
  )
import GettoUpload.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

index = "index.html" |> Href.internal QueryEncode.empty
