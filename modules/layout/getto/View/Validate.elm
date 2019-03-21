module GettoUpload.View.Validate exposing
  ( conflict
  , blank
  , noFile
  )

import Getto.Field.Validate as Validate

conflict = "conflict"

blank  = Validate.blank "blank"
noFile = Validate.empty "no-file"
