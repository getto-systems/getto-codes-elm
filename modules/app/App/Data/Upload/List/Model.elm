module GettoUpload.App.Data.Upload.List.Model exposing
  ( Frame
  , Transition
  , Msg
  , Page
  , Search
  )
import GettoUpload.App.Data.Upload.List.Search.View as Search
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.View.Http as HttpView

import Getto.Command.Transition as T
import Getto.Sort as Sort

type alias Frame = Frame.Model Layout.Model Page
type alias Transition msg = T.Transition Frame msg
type alias Msg msg = Frame.Msg Layout.Msg msg

type alias Page =
  { search : Search
  }

type alias Search =
  { form : Search.Form
  , page : Int
  , sort : Sort.Value
  , get  : HttpView.Model Search.Response
  }
