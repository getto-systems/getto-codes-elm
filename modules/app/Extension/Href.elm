module GettoCodes.Extension.Href exposing
  ( Href
  , Path(..)
  , path
  , internal
  , keycloak
  , toString
  )
import GettoCodes.Env as Env

import Getto.Url.Query.Encode as QueryEncode

type Href = Href
  { path  : Path
  , query : QueryEncode.Value
  }

type Path
  = Internal String
  | Keycloak String

path : Href -> Path
path (Href data) = data.path

internal : QueryEncode.Value -> String -> Href
internal query pathTo =
  Href
    { path  = Internal pathTo
    , query = query
    }

keycloak : QueryEncode.Value -> String -> Href
keycloak query pathTo =
  Href
    { path  = Keycloak pathTo
    , query = query
    }

toString : Href -> String
toString (Href data) =
  data.path |> prependRoot |> withQuery data.query

prependRoot : Path -> String
prependRoot pathTo =
  case pathTo of
    Internal url -> Env.href.internal ++ url
    Keycloak url -> Env.href.keycloak ++ url

withQuery : QueryEncode.Value -> String -> String
withQuery query pathData = pathData ++ (query |> QueryEncode.encode)
