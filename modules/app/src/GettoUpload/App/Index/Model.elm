module GettoUpload.App.Index.Model exposing
  ( Model
  , Init
  , init
  )
import GettoUpload.App.Index.Storage as Storage
import GettoUpload.App.Index.Query   as Query
import GettoUpload.Layout.Frame as Frame

type alias Model page = Frame.Model Storage.Model Query.Model page
type alias Init       = Frame.Init  Storage.Model Query.Model

init = (Storage.init,Query.init)
