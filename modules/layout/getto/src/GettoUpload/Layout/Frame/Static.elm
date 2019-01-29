module GettoUpload.Layout.Frame.Static exposing
  ( Flags
  , Model
  , Project
  , Page
  , init
  , project
  , page
  , version
  )

type alias Flags =
  { project : Project
  , page    : Page
  }

type Model = Model
  { project : Project
  , page    : Page
  , version : Version
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

type alias Version =
  { copyright : String
  , version   : String
  }

init : Version -> Flags -> Model
init versionData flags = Model
  { project = flags.project
  , page    = flags.page
  , version = versionData
  }

project : Model -> Project
project (Model data) = data.project

page : Model -> Page
page (Model data) = data.page

version : Model -> Version
version (Model data) = data.version
