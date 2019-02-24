module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , Init
  , View
  , State(..)
  , ResponseHeader
  , ResponseBody
  , ResponseInfo
  , ResponseDetail
  , edit
  , static
  , fromStateString
  , stateToString
  , done
  , compose
  , state
  , name
  , memo
  , age
  , email
  , tel
  , birthday
  , start_at
  , gender
  , quality
  , roles
  )
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

import File exposing ( File )
import Set exposing ( Set )

type alias Form =
  { state    : State
  , name     : Field.Model String
  , memo     : Field.Model String
  , age      : Field.Model String
  , email    : Field.Model String
  , tel      : Field.Model String
  , birthday : Field.Model String
  , start_at : Field.Model String
  , gender   : Field.Model String
  , quality  : Field.Model String
  , roles    : Field.Model (Set String)
  }

type alias Init =
  { name     : Validate.Init Form String
  , memo     : Validate.Init Form String
  , age      : Validate.Init Form String
  , email    : Validate.Init Form String
  , tel      : Validate.Init Form String
  , birthday : Validate.Init Form String
  , start_at : Validate.Init Form String
  , gender   : Validate.Init Form String
  , quality  : Validate.Init Form String
  , roles    : Validate.Init Form (Set String)
  }

type View = View State Bool
  { name     : Validate.Model (Form.Model Form String)
  , memo     : Validate.Model (Form.Model Form String)
  , age      : Validate.Model (Form.Model Form String)
  , email    : Validate.Model (Form.Model Form String)
  , tel      : Validate.Model (Form.Model Form String)
  , birthday : Validate.Model (Form.Model Form String)
  , start_at : Validate.Model (Form.Model Form String)
  , gender   : Validate.Model (Form.Model Form String)
  , quality  : Validate.Model (Form.Model Form String)
  , roles    : Validate.Model (Form.Model Form (Set String))
  }

type alias ResponseHeader = ()
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

type State
  = Static
  | Edit

static : Form -> Form
static form = { form | state = Static }

edit : Form -> Form
edit form = { form | state = Edit }

fromStateString : String -> Form -> Form
fromStateString stateString =
  case stateString |> String.toLower of
    "static" -> static
    "edit"   -> edit
    _        -> identity

stateToString : Form -> String
stateToString form =
  case form.state of
    Static -> "static"
    Edit   -> "edit"


done : HttpView.Migration ResponseHeader ResponseBody -> Form -> Form
done mig =
  case mig |> HttpView.isSuccess of
    Just _  -> static
    Nothing -> identity

compose : Init -> Form -> View
compose model form =
  View form.state
    ( List.concat
      [ model.name     |> Tuple.second
      , model.memo     |> Tuple.second
      , model.age      |> Tuple.second
      , model.email    |> Tuple.second
      , model.tel      |> Tuple.second
      , model.birthday |> Tuple.second
      , model.start_at |> Tuple.second
      , model.gender   |> Tuple.second
      , model.quality  |> Tuple.second
      , model.roles    |> Tuple.second
      ]
      |> List.any ((/=) Nothing)
    )
    { name     = form |> Validate.init model.name
    , memo     = form |> Validate.init model.memo
    , age      = form |> Validate.init model.age
    , email    = form |> Validate.init model.email
    , tel      = form |> Validate.init model.tel
    , birthday = form |> Validate.init model.birthday
    , start_at = form |> Validate.init model.start_at
    , gender   = form |> Validate.init model.gender
    , quality  = form |> Validate.init model.quality
    , roles    = form |> Validate.init model.roles
    }


state : View -> (State,Bool)
state (View st error _) = ( st, error )


name     (View st _ form) = ( st, form.name     |> Validate.expose )
memo     (View st _ form) = ( st, form.memo     |> Validate.expose )
age      (View st _ form) = ( st, form.age      |> Validate.expose )
email    (View st _ form) = ( st, form.email    |> Validate.expose )
tel      (View st _ form) = ( st, form.tel      |> Validate.expose )
birthday (View st _ form) = ( st, form.birthday |> Validate.expose )
start_at (View st _ form) = ( st, form.start_at |> Validate.expose )
gender   (View st _ form) = ( st, form.gender   |> Validate.expose )
quality  (View st _ form) = ( st, form.quality  |> Validate.expose )
roles    (View st _ form) = ( st, form.roles    |> Validate.expose )
