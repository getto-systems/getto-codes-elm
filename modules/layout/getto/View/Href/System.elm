module GettoCodes.View.Href.System exposing
  ( profile
  )
import GettoCodes.View.Href as Href

import Getto.Url.Query.Encode as QueryEncode

href params path = "system/" ++ path |> Href.internal params

profile = "profile.html" |> href QueryEncode.null
