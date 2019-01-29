module GettoUpload.Layout.Frame.Model exposing
  ( Model
  , init
  )
import GettoUpload.Layout.Frame.Credential as Credential
import GettoUpload.Layout.Frame.Page       as Page
import GettoUpload.Layout.Frame.Project    as Project
import GettoUpload.Layout.Command.Static.Entry as Static
import GettoUpload.Layout.Command.Auth.Entry   as Auth

type Model = Model (Inner)
type alias Inner =
  { credential : Auth.Model Credential.Model
  , page       : Static.Model Page.Model
  , project    : Static.Model Project.Model
  }

init : Model
init = Model
  { credential = Auth.init Credential.decode
  , page       = Static.init Page.decode
  , project    = Static.init Project.decode
  }
