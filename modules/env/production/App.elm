module GettoUpload.Env.App exposing
  ( root
  )

import GettoUpload.Version as Version

root = "/" ++ Version.data.version ++ "/"
