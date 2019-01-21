module GettoUpload.Layout.Model exposing
  ( Flags
  , Init
  , Model
  , Project
  , Credential
  , Static
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
    , page   : Decode.Value
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

type alias Model storage query page =
  { static : Static
  , layout : Layout
  , plugin : Plugin storage query
  , page   : page
  , command :
    { store  : StoreCommand.Model
    , search : SearchCommand.Model
    }
  }

type alias Static =
  { project : Project
  , path    : String
  , key     : Navigation.Key
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
  , page   : storage
  , encode : storage -> Encode.Value
  }

type alias Query query =
  { page   : query
  , encode : query -> QueryEncode.Value
  }