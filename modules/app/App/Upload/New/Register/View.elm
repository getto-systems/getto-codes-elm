module GettoUpload.App.Upload.New.Register.View exposing
  ( Form
  , View
  , Prop
  , update
  , prop
  , compose
  , hasError
  , name
  , text
  )

import Getto.Field as Field
import Getto.Field.View as FieldView

import File exposing ( File )

type alias Form =
  { name : Field.Model String
  , text : Field.Model (List File)
  }

type View = View Bool
  { name : Entry String
  , text : Entry (List File)
  }

type alias Entry a =
  { field : FieldView.Model a
  , prop  : Prop a
  }

type Prop a = Prop (Get a) (Set a)
type alias Get a = Form -> Field.Model a
type alias Set a = Field.Model a -> Form -> Form

type alias Validate a = ( Prop a, List (Maybe String) )


update : Prop a -> Field.Update a -> a -> Form -> Form
update (Prop get set) f value form =
  form |> set (form |> get |> f value)


prop : Get a -> Set a -> Prop a
prop = Prop

compose : Validate String -> Validate (List File) -> Form -> View
compose name_ text_ form =
  View
    ( List.concat
      [ name_ |> Tuple.second
      , text_ |> Tuple.second
      ]
      |> List.any ((/=) Nothing)
    )
    { name = form |> entry name_
    , text = form |> entry text_
    }

entry : Validate a -> Form -> Entry a
entry ((Prop get set),errors) form =
  let
    field = form |> get
  in
    { field = field |> FieldView.init (errors |> List.filterMap identity)
    , prop  = Prop get set
    }


hasError : View -> Bool
hasError (View error _) = error


name (View _ form) = form.name |> view
text (View _ form) = form.text |> view

view : Entry a -> ( FieldView.Model a, Prop a )
view m = ( m.field, m.prop )