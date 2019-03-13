module GettoUpload.Layout.Page.Model exposing
  ( Frame
  , Transition
  , Page
  , Article
  , Side
  , Options
  )
import GettoUpload.Layout.Page.Side.View as Side
import GettoUpload.Layout.Page.Options.View as Options
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
  , options : Options
  }

type alias Article = ()

type alias Side =
  { menu       : Menu
  , get        : HttpView.Model Side.Response
  , badgeNames : Dict String String
  , collapsed  : Set String
  }

type alias Options =
  { get : HttpView.Model Options.Response
  }
