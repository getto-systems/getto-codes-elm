module GettoCodes.App.Href exposing
  ( index
  )
import GettoCodes.View.Href as Href

import Getto.Url.Query.Encode as QueryEncode

index = "index.html" |> Href.internal QueryEncode.null
