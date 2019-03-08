module GettoUpload.App.Upload.Edit.Model exposing
  ( Data
  , pathInfo
  , etag
  )
import GettoUpload.App.Upload.Edit.Data.View as Data
import GettoUpload.View.Http as HttpView

type alias Data =
  { id  : Int
  , get : HttpView.Model Data.Response
  }

pathInfo : Data -> List ( String, String )
pathInfo model =
  [ ( "id", model.id |> String.fromInt )
  ]

etag : Data -> Maybe String
etag model = model.get |> HttpView.response |> Maybe.map (HttpView.header >> .etag)
