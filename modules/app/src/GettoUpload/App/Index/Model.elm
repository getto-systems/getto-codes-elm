module GettoUpload.App.Index.Model exposing
  ( Model
  , Init
  , init
  )
import GettoUpload.App.Index.Store  as Store
import GettoUpload.App.Index.Search as Search
import GettoUpload.Layout.Frame as Frame

type alias Model page = Frame.Model Store.Model Search.Model page
type alias Init       = Frame.Init  Store.Model Search.Model

init = (Store.init,Search.init)
