module GettoUpload.Layout.Model exposing
  ( Flags
  , Init
  , Model
  , Project
  , Credential
  , Static
  , Page
  , Layout
  , Plugin
  , Storage
  , Query
  )
import GettoUpload.Layout.Command.Store  as StoreCommand
import GettoUpload.Layout.Command.Search as SearchCommand
import GettoUpload.Layout.Storage as Storage

import Getto.Url.Query.Encode as QueryEncode

import Browser.Navigation as Navigation
import Url exposing ( Url )
import Json.Encode as Encode
import Json.Decode as Decode


type alias Flags =
  { project    : Project
  , path       : String
  , credential : Credential
  , storage :
    { layout : Decode.Value
    , app    : Decode.Value
    }
  }

type alias Project =
  { name    : String
  , company : String
  , title   : String
  , sub     : String
  }

type alias Credential =
  { token : String
  , roles : List String
  }

type alias Init storage query =
  { static : Static
  , layout : Layout
  , plugin : Plugin storage query
  , command :
    { store  : StoreCommand.Model
    , search : SearchCommand.Model
    }
  }

type alias Model storage query app =
  { static : Static
  , layout : Layout
  , plugin : Plugin storage query
  , app    : app
  , command :
    { store  : StoreCommand.Model
    , search : SearchCommand.Model
    }
  }

type alias Static =
  { project : Project
  , page    : Page
  , key     : Navigation.Key
  }

type alias Page =
  { path : String
  }

type alias Layout =
  { url        : Url
  , credential : Credential
  }

type alias Plugin storage query =
  { storage : Storage storage
  , query   : Query query
  }

type alias Storage storage =
  { layout : Storage.Model
  , app    : storage
  , encode : storage -> Encode.Value
  }

type alias Query query =
  { app    : query
  , encode : query -> QueryEncode.Value
  }
