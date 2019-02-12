module GettoUpload.App.Upload.New.Register.View exposing
  ( Form
  , View
  , Prop
  , input
  , check
  , prop
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
  )

import Getto.Field as Field
import Getto.Field.View as FieldView

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
  --, roles    : Field.Model (Set String) -- checkbox
  }

type View = View Bool
  { name     : Entry String
  , text     : Entry (List File)
  , memo     : Entry String
  , age      : Entry String
  , email    : Entry String
  , tel      : Entry String
  , birthday : Entry String
  , start_at : Entry String
  , gender   : Entry String
  , quality  : Entry String
  }

type alias Entry a =
  { field : FieldView.Model a
  , prop  : Prop a
  }

type Prop a = Prop (Getter a) (Setter a)
type alias Getter a = Form -> Field.Model a
type alias Setter a = Field.Model a -> Form -> Form

type alias Validate a = ( Prop a, List (Maybe String) )


input : Prop a -> a -> Form -> Form
input (Prop get set) value form =
  form |> set (form |> get |> Field.update value)

check : Prop (Set comparable) -> comparable -> Bool -> Form -> Form
check (Prop get set) value checked form =
  form |> set (form |> get |> Field.check value checked)


prop : Getter a -> Setter a -> Prop a
prop = Prop

compose : Validate String -> Validate (List File) -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Validate String -> Form -> View
compose name_ text_ memo_ age_ email_ tel_ birthday_ start_at_ gender_ quality_ form =
  View
    ( List.concat
      [ name_     |> Tuple.second
      , text_     |> Tuple.second
      , memo_     |> Tuple.second
      , age_      |> Tuple.second
      , email_    |> Tuple.second
      , tel_      |> Tuple.second
      , birthday_ |> Tuple.second
      , start_at_ |> Tuple.second
      , gender_   |> Tuple.second
      , quality_  |> Tuple.second
      ]
      |> List.any ((/=) Nothing)
    )
    { name     = form |> entry name_
    , text     = form |> entry text_
    , memo     = form |> entry memo_
    , age      = form |> entry age_
    , email    = form |> entry email_
    , tel      = form |> entry tel_
    , birthday = form |> entry birthday_
    , start_at = form |> entry start_at_
    , gender   = form |> entry gender_
    , quality  = form |> entry quality_
    }

entry : Validate a -> Form -> Entry a
entry ((Prop get set),errors) form =
  { field = form |> get |> FieldView.init (errors |> List.filterMap identity)
  , prop  = Prop get set
  }


hasError : View -> Bool
hasError (View error _) = error


name     (View _ form) = form.name     |> view
text     (View _ form) = form.text     |> view
memo     (View _ form) = form.memo     |> view
age      (View _ form) = form.age      |> view
email    (View _ form) = form.email    |> view
tel      (View _ form) = form.tel      |> view
birthday (View _ form) = form.birthday |> view
start_at (View _ form) = form.start_at |> view
gender   (View _ form) = form.gender   |> view
quality  (View _ form) = form.quality  |> view

view : Entry a -> ( FieldView.Model a, Prop a )
view m = ( m.field, m.prop )
