module GettoUpload.Env.App exposing
  ( href
  , api
  )

href =
  { internal = "/dist/"
  , keycloak = "http://getto.workstation:10380/"
  }

api =
  { upload = "http://getto.workstation:30080/"
  }
