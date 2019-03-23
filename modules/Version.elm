module GettoCodes.Version exposing
  ( Data
  , data
  )

type alias Data =
  { copyright : String
  , version   : String
  }

data : Data
data =
  { copyright = "GETTO systems"
  , version = "0.1.0"
  }
