module GettoUpload.App.Upload.New.Register.View exposing
  ( Form
  , Init
  , View
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

type alias Form =
  { name     : Field.Model String
  , text     : Field.Model (List File)
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
  , text     : Validate.Init Form (List File)
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

type View = View Bool
  { name     : Validate.Model (Form.Model Form String)
  , text     : Validate.Model (Form.Model Form (List File))
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

type alias ResponseHeader =
  { id : Int
  }
type alias ResponseBody = ()

compose : Init -> Form -> View
compose model form =
  View
    ( List.concat
      [ model.name     |> Tuple.second
      , model.text     |> Tuple.second
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
    , text     = form |> Validate.init model.text
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


hasError : View -> Bool
hasError (View error _) = error


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
