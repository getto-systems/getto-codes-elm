module GettoUpload.Env.App exposing
  ( href
  , api
  )
import GettoUpload.Version as Version

href =
  { internal = "/" ++ Version.data.version ++ "/"
  , keycloak = "https://keycloak.getto.systems/"
  }

api =
  { host = "https://api.upload.getto.systems/"
  }
