module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , State(..)
  , View
  , ResponseHeader
  , ResponseBody
  , ResponseInfo
  , ResponseDetail
  , Prop
  , edit
  , static
  , update
  , toggle
  , prop
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

import Getto.Field as Field
import Getto.Field.View as FieldView

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

type View = View State Bool
  { name     : Entry String
  , memo     : Entry String
  , age      : Entry String
  , email    : Entry String
  , tel      : Entry String
  , birthday : Entry String
  , start_at : Entry String
  , gender   : Entry String
  , quality  : Entry String
  , roles    : Entry (Set String)
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

type alias Entry a =
  { field : FieldView.Model a
  , prop  : Prop a
  }

type Prop a = Prop (Getter a) (Setter a)
type alias Getter a = Form -> Field.Model a
type alias Setter a = Field.Model a -> Form -> Form

type alias Validate a = ( Prop a, List (Maybe String) )

edit : Form -> Form
edit form = { form | state = Edit }

static : Form -> Form
static form = { form | state = Static }

update : Prop a -> a -> Form -> Form
update (Prop get set) value form =
  form |> set (form |> get |> Field.update value)

toggle : Prop (Set comparable) -> comparable -> Form -> Form
toggle (Prop get set) value form =
  form |> set (form |> get |> Field.toggle value)


prop : Getter a -> Setter a -> Prop a
prop = Prop

compose : Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate (Set String) -> Form -> View
compose name_ memo_ age_ email_ tel_ birthday_ start_at_ gender_ quality_ roles_ form =
  View form.state
    ( List.concat
      [ name_     |> Tuple.second
      , memo_     |> Tuple.second
      , age_      |> Tuple.second
      , email_    |> Tuple.second
      , tel_      |> Tuple.second
      , birthday_ |> Tuple.second
      , start_at_ |> Tuple.second
      , gender_   |> Tuple.second
      , quality_  |> Tuple.second
      , roles_    |> Tuple.second
      ]
      |> List.any ((/=) Nothing)
    )
    { name     = form |> entry name_
    , memo     = form |> entry memo_
    , age      = form |> entry age_
    , email    = form |> entry email_
    , tel      = form |> entry tel_
    , birthday = form |> entry birthday_
    , start_at = form |> entry start_at_
    , gender   = form |> entry gender_
    , quality  = form |> entry quality_
    , roles    = form |> entry roles_
    }

entry : Validate a -> Form -> Entry a
entry ((Prop get set),errors) form =
  { field = form |> get |> FieldView.init (errors |> List.filterMap identity)
  , prop  = Prop get set
  }


state : View -> (State,Bool)
state (View st error _) = ( st, error )


name     (View st _ form) = form.name     |> view st
memo     (View st _ form) = form.memo     |> view st
age      (View st _ form) = form.age      |> view st
email    (View st _ form) = form.email    |> view st
tel      (View st _ form) = form.tel      |> view st
birthday (View st _ form) = form.birthday |> view st
start_at (View st _ form) = form.start_at |> view st
gender   (View st _ form) = form.gender   |> view st
quality  (View st _ form) = form.quality  |> view st
roles    (View st _ form) = form.roles    |> view st

view : State -> Entry a -> ( State, FieldView.Model a, Prop a )
view st m = ( st, m.field, m.prop )
