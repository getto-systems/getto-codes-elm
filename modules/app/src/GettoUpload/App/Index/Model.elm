module GettoUpload.App.Index.Model exposing
  ( Model
  , Init
  , Msg
  , init
  )
import GettoUpload.App.Index.Store  as Store
import GettoUpload.App.Index.Search as Search
import GettoUpload.App.Index.Http   as Http
import GettoUpload.Layout.Frame as Frame

type alias Model page = Frame.Model Store.Model Search.Model Http.Model page
type alias Init       = Frame.Init  Store.Model Search.Model Http.Model

type alias Msg msg = Frame.Msg Http.Model msg

init =
  ( Search.init
  , Store.init
  , Http.init
  )
