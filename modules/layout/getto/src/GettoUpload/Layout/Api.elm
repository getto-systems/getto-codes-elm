module GettoUpload.Layout.Api exposing
  ( prependRoot
  )
import GettoUpload.Env.App as Env

prependRoot : String -> String
prependRoot path = Env.apiRoot ++ path
