module GettoUpload.App.Upload.New.Register.View exposing
  ( Form
  , Init
  , View
  , Prop
  , ResponseHeader
  , ResponseBody
  , compose
  , hasError
  , name
  , text
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

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

import File exposing ( File )
import Set exposing ( Set )

type alias Attribute = ()
type alias Prop      a = Form.Prop Form Attribute a
type alias Field     a = Field.Model Attribute a
type alias InitForm  a = Validate.Init Form Attribute a
type alias ViewModel a = Validate.Model (Form.Model Form Attribute a)

type alias Form =
  { name     : Field String
  , text     : Field (List File)
  , memo     : Field String
  , age      : Field String
  , email    : Field String
  , tel      : Field String
  , birthday : Field String
  , start_at : Field String
  , gender   : Field String
  , quality  : Field String
  , roles    : Field (Set String)
  }

type alias Init =
  { name     : InitForm String
  , text     : InitForm (List File)
  , memo     : InitForm String
  , age      : InitForm String
  , email    : InitForm String
  , tel      : InitForm String
  , birthday : InitForm String
  , start_at : InitForm String
  , gender   : InitForm String
  , quality  : InitForm String
  , roles    : InitForm (Set String)
  }

type View = View
  { hasError : Bool
  }
  { name     : ViewModel String
  , text     : ViewModel (List File)
  , memo     : ViewModel String
  , age      : ViewModel String
  , email    : ViewModel String
  , tel      : ViewModel String
  , birthday : ViewModel String
  , start_at : ViewModel String
  , gender   : ViewModel String
  , quality  : ViewModel String
  , roles    : ViewModel (Set String)
  }

type alias ResponseHeader =
  { id : Int
  }
type alias ResponseBody = ()

compose : Init -> Form -> View
compose model form =
  let
    view =
      { name     = form |> Validate.init Validate.none model.name
      , text     = form |> Validate.init Validate.none model.text
      , memo     = form |> Validate.init Validate.none model.memo
      , age      = form |> Validate.init Validate.none model.age
      , email    = form |> Validate.init Validate.none model.email
      , tel      = form |> Validate.init Validate.none model.tel
      , birthday = form |> Validate.init Validate.none model.birthday
      , start_at = form |> Validate.init Validate.none model.start_at
      , gender   = form |> Validate.init Validate.none model.gender
      , quality  = form |> Validate.init Validate.none model.quality
      , roles    = form |> Validate.init Validate.none model.roles
      }
    errors = List.concat
      [ view.name     |> Validate.errors
      , view.text     |> Validate.errors
      , view.memo     |> Validate.errors
      , view.age      |> Validate.errors
      , view.email    |> Validate.errors
      , view.tel      |> Validate.errors
      , view.birthday |> Validate.errors
      , view.start_at |> Validate.errors
      , view.gender   |> Validate.errors
      , view.quality  |> Validate.errors
      , view.roles    |> Validate.errors
      ]
  in
    View
      { hasError = errors |> List.isEmpty |> not
      }
      view


hasError : View -> Bool
hasError (View info _) = info.hasError


name     (View _ form) = form.name     |> Validate.expose
text     (View _ form) = form.text     |> Validate.expose
memo     (View _ form) = form.memo     |> Validate.expose
age      (View _ form) = form.age      |> Validate.expose
email    (View _ form) = form.email    |> Validate.expose
tel      (View _ form) = form.tel      |> Validate.expose
birthday (View _ form) = form.birthday |> Validate.expose
start_at (View _ form) = form.start_at |> Validate.expose
gender   (View _ form) = form.gender   |> Validate.expose
quality  (View _ form) = form.quality  |> Validate.expose
roles    (View _ form) = form.roles    |> Validate.expose
