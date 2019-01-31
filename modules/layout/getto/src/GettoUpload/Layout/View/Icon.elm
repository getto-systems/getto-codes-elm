module GettoUpload.Layout.View.Icon exposing
  ( Icon(..)
  , Fa(..)
  , fas
  , far
  )

type Icon
  = Fa Fa

type Fa
  = Solid String
  | Regular String

fas : String -> Icon
fas = Solid >> Fa

far : String -> Icon
far = Regular >> Fa
