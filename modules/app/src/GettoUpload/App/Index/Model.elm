module GettoUpload.App.Index.Model exposing
  ( Model
  , Init
  , init
  )
import GettoUpload.App.Index.Storage as Storage
import GettoUpload.App.Index.Query   as Query
import GettoUpload.Layout.Model as Model

type alias Model page = Model.Model Storage.Model Query.Model page
type alias Init       = Model.Init  Storage.Model Query.Model

init = (Storage.init,Query.init)
