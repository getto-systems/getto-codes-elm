module GettoUpload.App.Upload.Edit.Data.View exposing
  ( Response
  , ResponseHeader
  , ResponseBody
  , ResponseInfo
  , ResponseDetail
  , etag
  )
import GettoUpload.View.Http as HttpView

import Set exposing ( Set )

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { etag : String
  }
type alias ResponseBody =
  { info   : ResponseInfo
  , detail : ResponseDetail
  }
type alias ResponseInfo =
  { name     : String
  , memo     : String
  , age      : Int
  , email    : String
  , tel      : String
  }
type alias ResponseDetail =
  { birthday : String
  , start_at : String
  , gender   : String
  , quality  : String
  , roles    : Set String
  }

etag : Response -> String
etag = HttpView.header >> .etag
