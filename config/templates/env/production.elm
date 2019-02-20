module GettoUpload.Env exposing
  ( href
  , api
  )
import GettoUpload.Version as Version

href =
  { internal = "/" ++ Version.data.version ++ "/"
  , keycloak = "$KEYCLOAK_URL/"
  }

api =
  { host = "$API_URL/"
  }
