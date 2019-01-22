module GettoUpload.Layout.Command.Static exposing
  ( Flags
  , Project
  , Page
  )

type alias Flags =
  { project : Project
  , page    : Page
  }

type alias Project =
  { name    : String
  , company : String
  , title   : String
  , sub     : String
  }

type alias Page =
  { path : String
  }
