module GettoCodes.App.System.Profile.Model exposing
  ( Frame
  , Transition
  , Msg
  , Page
  , Dashboard
  )
import GettoCodes.Layout.Frame as Frame
import GettoCodes.Layout.Page.Page as Layout

import Getto.Command.Transition as T

type alias Frame = Frame.Model Layout.Model Page
type alias Transition msg = T.Transition Frame msg
type alias Msg msg = Frame.Msg Layout.Msg msg

type alias Page =
  { dashboard : Dashboard
  }

type alias Dashboard = ()
