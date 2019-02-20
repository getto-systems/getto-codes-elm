module GettoUpload.Env exposing
  ( href
  , api
  )

href =
  { internal = "/dist/"
  , keycloak = "$KEYCLOAK_URL/"
  }

api =
  { host = "$API_URL/"
  }
