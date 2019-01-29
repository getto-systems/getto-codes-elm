module GettoUpload.Layout.Frame.Http.Data exposing
  ( Model
  )
import GettoUpload.Layout.Frame.Credential as Credential

type alias Model =
  { credential : Credential.Model
  }
