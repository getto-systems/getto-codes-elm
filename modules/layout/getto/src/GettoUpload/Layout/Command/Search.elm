module GettoUpload.Layout.Command.Search exposing
  ( Model
  , Init
  , init
  , exec
  , changed
  , query
  , update
  )

import Getto.Url.Query.Encode as QueryEncode
import Getto.Url.Query.Decode as QueryDecode

import Browser.Navigation as Navigation
import Url exposing ( Url )

type Model query = Model (Entry query)

type alias Entry query =
  { key    : Navigation.Key
  , query  : query
  , encode : query -> QueryEncode.Value
  , decode : List String -> query
  , cmd    : PushUrl
  }

type PushUrl
  = None
  | PushUrl

type alias Init query = ( List String -> query, query -> QueryEncode.Value )

init : Init query -> ( Url, Navigation.Key ) -> Model query
init (decode,encode) (url,key) =
  Model
    { key    = key
    , query  = url |> decodeQuery decode
    , encode = encode
    , decode = decode
    , cmd    = None
    }

exec : Model query -> ( Model query, Cmd msg )
exec (Model model) =
  case model.cmd of
    None    -> ( Model model, Cmd.none )
    PushUrl ->
      ( Model { model | cmd = None }
      , model.query |> model.encode |> QueryEncode.encode |> Navigation.pushUrl model.key
      )

changed : Url -> Model query -> Model query
changed url (Model model) =
  Model { model | query = url |> decodeQuery model.decode }

decodeQuery : (List String -> query) -> Url -> query
decodeQuery f url =
  url.query
  |> Maybe.map QueryDecode.split
  |> Maybe.withDefault []
  |> f

query : Model query -> query
query (Model model) = model.query

update : (query -> query) -> Model query -> Model query
update f (Model model) =
  Model
    { model
    | query = model.query |> f
    , cmd   = PushUrl
    }
