module GettoUpload.Layout.Model exposing
  ( Flags
  , Init
  , Model
  , Static
  , Project
  , Page
  )
import GettoUpload.Layout.Command.Credential as Credential
import GettoUpload.Layout.Command.Store      as Store
import GettoUpload.Layout.Command.Search     as Search
import GettoUpload.Layout.Store as LayoutStore


type alias Flags =
  { static     : Static
  , credential : Credential.Flags
  , store      : Store.Flags
  }

type alias Base store search a =
  { a
  | static     : Static
  , credential : Credential.Model
  , store      : Store.Model LayoutStore.Model store
  , search     : Search.Model search
  }

type alias Init  store search     = Base store search {}
type alias Model store search app = Base store search { app : app }

type alias Static =
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
