module GettoUpload.App.Data.Upload.Edit.Model exposing
  ( Frame
  , Transition
  , Msg
  , Page
  , Data
  , Info
  , Detail
  , Complete
  , Unregister
  , pathInfo
  , etag
  )
import GettoUpload.App.Data.Upload.Edit.Data.View       as Data
import GettoUpload.App.Data.Upload.Edit.Info.View       as Info
import GettoUpload.App.Data.Upload.Edit.Detail.View     as Detail
import GettoUpload.App.Data.Upload.Edit.Complete.View   as Complete
import GettoUpload.App.Data.Upload.Edit.Unregister.View as Unregister
import GettoUpload.Layout.Frame as Frame
import GettoUpload.Layout.Page.Page as Layout
import GettoUpload.View.Http as HttpView

import Getto.Command.Transition as T

type alias Frame = Frame.Model Layout.Model Page
type alias Transition msg = T.Transition Frame msg
type alias Msg msg = Frame.Msg Layout.Msg msg

type alias Page =
  { data       : Data
  , info       : Info
  , detail     : Detail
  , complete   : Complete
  , unregister : Unregister
  }

type alias Data =
  { id  : Int
  , get : HttpView.Model Data.Response
  }

type alias Info =
  { form : Info.Form
  , put  : HttpView.Model Info.Response
  }

type alias Detail =
  { form : Detail.Form
  , put  : HttpView.Model Detail.Response
  }

type alias Complete =
  { isEdit      : Bool
  , putComplete : HttpView.Model Complete.Response
  , putWork     : HttpView.Model Complete.Response
  }

type alias Unregister =
  { isConfirm : Bool
  , delete    : HttpView.Model Unregister.Response
  }

pathInfo : Data -> List ( String, String )
pathInfo model =
  [ ( "id", model.id |> String.fromInt )
  ]

etag : Data -> Maybe String
etag model = model.get |> HttpView.response |> Maybe.map (HttpView.header >> .etag)
