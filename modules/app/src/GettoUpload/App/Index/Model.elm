module GettoUpload.App.Index.Model exposing
  ( Model
  , Init
  , init
  )
import GettoUpload.App.Index.Store  as Store
import GettoUpload.App.Index.Search as Search
import GettoUpload.Layout.Model as Model

type alias Model page = Model.Model Store.Model Search.Model page
type alias Init       = Model.Init  Store.Model Search.Model

init = (Store.init,Search.init)
