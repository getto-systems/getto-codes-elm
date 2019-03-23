module GettoCodes.View.Validate exposing
  ( conflict
  , blank
  , noFile
  )

import Getto.Field.Validate as Validate

conflict = "conflict"

blank  = Validate.blank "blank"
noFile = Validate.emptyList "no-file"
