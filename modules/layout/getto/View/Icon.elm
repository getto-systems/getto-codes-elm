module GettoUpload.View.Icon exposing
  ( Icon(..)
  , Fa(..)
  , fas
  , far
  , edit
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

edit : Icon
edit = far "edit"
