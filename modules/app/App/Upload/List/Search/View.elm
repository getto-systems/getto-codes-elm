module GettoUpload.App.Upload.List.Search.View exposing
  ( Form
  , Init
  , View
  , Prop
  , Response
  , ResponseHeader
  , ResponseBody
  , Upload
  , Comment
  , Like
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
import GettoUpload.View.Http as HttpView

import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Present as Present

import Set exposing ( Set )

type alias Attribute = ()
type alias Prop      a = Form.Prop Form Attribute a
type alias Field     a = Field.Model Attribute a
type alias InitForm  a = Present.Init Form Attribute a

type alias ViewModel    a = Present.Model (Form.Model   Form Attribute a)
type alias BetweenModel a = Present.Model (Form.Between Form Attribute a)

type alias Form =
  { name          : Field String
  , age_gteq      : Field String
  , age_lteq      : Field String
  , email         : Field String
  , tel           : Field String
  , birthday_gteq : Field String
  , birthday_lteq : Field String
  , start_at_gteq : Field String
  , start_at_lteq : Field String
  , gender        : Field String
  , roles         : Field (Set String)
  }

type alias Init =
  { name          : InitForm String
  , age_gteq      : InitForm String
  , age_lteq      : InitForm String
  , email         : InitForm String
  , tel           : InitForm String
  , birthday_gteq : InitForm String
  , birthday_lteq : InitForm String
  , start_at_gteq : InitForm String
  , start_at_lteq : InitForm String
  , gender        : InitForm String
  , roles         : InitForm (Set String)
  }

type View = View
  { name     : ViewModel String
  , email    : ViewModel String
  , tel      : ViewModel String
  , gender   : ViewModel String
  , roles    : ViewModel (Set String)
  , age      : BetweenModel String
  , birthday : BetweenModel String
  , start_at : BetweenModel String
  }

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { max : Int
  }
type alias ResponseBody = List Upload
type alias Upload =
  { id       : Int
  , name     : String
  , gender   : String
  , roles    : List String
  , comments : List Comment
  }
type alias Comment =
  { user : String
  , text : String
  , likes : List Like
  }
type alias Like =
  { user : String
  , text : String
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
