module GettoUpload.Layout.View.Icon exposing
  ( Icon(..)
  , Fa(..)
  , fas
  , far
  )

type Icon
  = Fa Fa

type Fa
  = FaSolid String
  | FaRegular String

fas : String -> Icon
fas = FaSolid >> Fa

far : String -> Icon
far = FaRegular >> Fa
