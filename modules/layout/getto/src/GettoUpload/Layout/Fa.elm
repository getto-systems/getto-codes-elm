module GettoUpload.Layout.Fa exposing
  ( Icon
  , solid
  , regular
  )

type Icon
  = Solid String
  | Regular String

solid : String -> Icon
solid = Solid

regular : String -> Icon
regular = Regular
