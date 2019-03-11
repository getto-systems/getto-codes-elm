module GettoUpload.Layout.Page.Model exposing
  ( Frame
  , Transition
  , Page
  , Article
  , Side
  )
import GettoUpload.Layout.Page.Side.View as Side
import GettoUpload.Layout.Frame as Frame
import GettoUpload.View.Http as HttpView
import GettoUpload.View.Menu exposing ( Menu )

import Getto.Command.Transition as T

import Set exposing ( Set )
import Dict exposing ( Dict )

type alias Frame model = Frame.Model Page model
type alias Transition model msg = T.Transition (Frame model) msg

type alias Page =
  { article : Article
  , side    : Side
  }

type alias Article = ()

type alias Side =
  { menu       : Menu
  , badge      : HttpView.Model Side.Response
  , badgeNames : Dict String String
  , collapsed  : Set String
  }
