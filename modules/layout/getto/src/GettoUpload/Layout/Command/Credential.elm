port module GettoUpload.Layout.Command.Credential exposing
  ( Flags
  , Model
  , init
  , exec
  , changed
  , token
  , roles
  , logout
  )

port clearCredential : () -> Cmd msg

type alias Flags =
  { token : String
  , roles : List String
  }

type Model = Model Entry

type alias Entry =
  { credential : Flags
  , cmd        : Logout
  }

type Logout
  = None
  | Logout

init : Flags -> Model
init flags =
  Model
    { credential = flags
    , cmd        = None
    }

exec : Model -> ( Model, Cmd msg )
exec (Model model) =
  case model.cmd of
    None   -> ( Model model, Cmd.none )
    Logout ->
      ( Model { model | cmd = None }
      , clearCredential ()
      )

changed : Flags -> Model -> Model
changed credential (Model model) =
  Model { model | credential = credential }

token : Model -> String
token (Model model) = model.credential.token

roles : Model -> List String
roles (Model model) = model.credential.roles

logout : Model -> Model
logout (Model model) =
  Model { model | cmd = Logout }
