module GettoCodes.Extension.Href.Home exposing
  ( index
  )
import GettoCodes.Extension.Href as Href

import Getto.Url.Query.Encode as QueryEncode

index = "index.html" |> Href.internal QueryEncode.null
