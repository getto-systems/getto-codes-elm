module GettoUpload.Layout.Http exposing
  ( request
  , track
  )
import GettoUpload.Layout.Http.Mock as Mock

import Getto.Http.RequestMock as Request

request = Request.request Mock.find
track   = Request.track
