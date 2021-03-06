module GettoCodes.App.Data.Upload.New.Model exposing
  ( Frame
  , Transition
  , Msg
  , Page
  , Register
  )
import GettoCodes.App.Data.Upload.New.Register.View as Register
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Page.Page as Layout
import GettoCodes.View.Http as HttpView

import Getto.Command.Transition as T

type alias Frame = Frame.Model Layout.Model Page
type alias Transition msg = T.Transition Frame msg
type alias Msg msg = Frame.Msg Layout.Msg msg

type alias Page =
  { register : Register
  }

type alias Register =
  { tmpId  : Maybe Int
  , form   : Register.Form
  , upload : HttpView.Model Register.Response
  }
