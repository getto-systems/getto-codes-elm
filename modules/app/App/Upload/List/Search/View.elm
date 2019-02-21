module GettoUpload.App.Upload.List.Search.View exposing
  ( Form
  , Init
  , View
  , ResponseHeader
  , ResponseBody
  , Upload
  , compose
  , name
  , age
  , email
  , tel
  , birthday
  , start_at
  , gender
  , roles
  )

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present

import Set exposing ( Set )

type alias Form =
  { name          : Field.Model String
  , age_gteq      : Field.Model String
  , age_lteq      : Field.Model String
  , email         : Field.Model String
  , tel           : Field.Model String
  , birthday_gteq : Field.Model String
  , birthday_lteq : Field.Model String
  , start_at_gteq : Field.Model String
  , start_at_lteq : Field.Model String
  , gender        : Field.Model String
  , roles         : Field.Model (Set String)
  }

type alias Init =
  { name          : Present.Init Form String
  , age_gteq      : Present.Init Form String
  , age_lteq      : Present.Init Form String
  , email         : Present.Init Form String
  , tel           : Present.Init Form String
  , birthday_gteq : Present.Init Form String
  , birthday_lteq : Present.Init Form String
  , start_at_gteq : Present.Init Form String
  , start_at_lteq : Present.Init Form String
  , gender        : Present.Init Form String
  , roles         : Present.Init Form (Set String)
  }

type View = View
  { name     : Present.Model (Form.Model Form String)
  , email    : Present.Model (Form.Model Form String)
  , tel      : Present.Model (Form.Model Form String)
  , gender   : Present.Model (Form.Model Form String)
  , roles    : Present.Model (Form.Model Form (Set String))
  , age      : Present.Model (Form.Between Form String)
  , birthday : Present.Model (Form.Between Form String)
  , start_at : Present.Model (Form.Between Form String)
  }

type alias ResponseHeader =
  { max : Int
  }
type alias ResponseBody = List Upload
type alias Upload =
  { id     : Int
  , name   : String
  , gender : String
  , roles  : List String
  }


compose : Init -> Form -> View
compose model form = View
  { name     = form |> Present.init model.name
  , email    = form |> Present.init model.email
  , tel      = form |> Present.init model.tel
  , gender   = form |> Present.init model.gender
  , roles    = form |> Present.init model.roles
  , age      = form |> Present.between "age"      { gteq = model.age_gteq,      lteq = model.age_lteq }
  , birthday = form |> Present.between "birthday" { gteq = model.birthday_gteq, lteq = model.birthday_lteq }
  , start_at = form |> Present.between "start_at" { gteq = model.start_at_gteq, lteq = model.start_at_lteq }
  }


name     (View form) = form.name     |> Present.expose
age      (View form) = form.age      |> Present.expose
email    (View form) = form.email    |> Present.expose
tel      (View form) = form.tel      |> Present.expose
birthday (View form) = form.birthday |> Present.expose
start_at (View form) = form.start_at |> Present.expose
gender   (View form) = form.gender   |> Present.expose
roles    (View form) = form.roles    |> Present.expose
