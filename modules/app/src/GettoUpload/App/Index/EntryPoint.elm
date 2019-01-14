module GettoUpload.App.Index.EntryPoint exposing (main)
import GettoUpload.App.Index.Page as Page

import Browser

main = Browser.application
  { init          = Page.init
  , subscriptions = Page.subscriptions
  , onUrlRequest  = Page.onUrlRequest
  , onUrlChange   = Page.onUrlChange
  , update        = Page.update
  , view          = Page.view
  }
