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
import GettoUpload.Extension.Href as Href exposing ( Href )

import Getto.Url.Query.Encode as QueryEncode

type alias Flags =
  { project : Project
  , page    : PageFlags
  }

type alias PageFlags =
  { path : String
  }

type Model = Model
  { version : Version
  , project : Project
  , page    : Page
  }

type alias Version =
  { copyright : String
  , version   : String
  }

type alias Project =
  { name    : String
  , company : String
  , title   : String
  , sub     : String
  }

type alias Page =
  { path : String
  , href : Href
  }

init : Version -> Flags -> Model
init versionData flags = Model
  { version = versionData
  , project = flags.project
  , page    =
    { path = flags.page.path
    , href = flags.page.path |> Href.internal QueryEncode.empty
    }
  }

project : Model -> Project
project (Model model) = model.project

page : Model -> Page
page (Model model) = model.page

version : Model -> Version
version (Model model) = model.version
