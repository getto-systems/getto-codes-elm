module GettoUpload.Env.App exposing
  ( root
  )

import GettoUpload.Layout.Version as Version

root = "/" ++ Version.version ++ "/"
