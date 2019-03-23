module GettoCodes.View.Icon exposing
  ( Icon(..)
  , Fa(..)
  , fas
  , far
  , spinner
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

spinner : Icon
spinner = fas "spinner"

edit : Icon
edit = far "edit"
