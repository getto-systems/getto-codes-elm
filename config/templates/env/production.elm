module GettoCodes.Env exposing
  ( href
  , api
  )
import GettoCodes.Version as Version

href =
  { internal = "/" ++ Version.data.version ++ "/"
  , keycloak = "$KEYCLOAK_URL/"
  }

api =
  { host = "$API_URL/"
  }
