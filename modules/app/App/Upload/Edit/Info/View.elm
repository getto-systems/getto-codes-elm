module GettoUpload.App.Upload.Edit.Info.View exposing
  ( Form
  , Init
  , View
  , Prop
  , State(..)
  , Response
  , ResponseHeader
  , ResponseBody
  , ResponseInfo
  , ResponseDetail
  , static
  , toStatic
  , toEdit
  , fromStateString
  , stateToString
  , done
  , compose
  , last
  , isStatic
  , hasError
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
import Getto.Field.Conflict as Conflict

import File exposing ( File )
import Set exposing ( Set )

type alias Prop      a = Conflict.Prop Form a
type alias Field     a = Conflict.Field a
type alias InitForm  a = Conflict.Init ResponseBody Form a
type alias ViewModel a = Validate.Model (Conflict.Form Form a)

type alias Form =
  { state    : EditState
  , name     : Field String
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
  { state    : EditState
  , last     : Maybe Response
  , hasError : Bool
  }
  { name     : ViewModel String
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

type alias Response = HttpView.Response ResponseHeader ResponseBody
type alias ResponseHeader =
  { etag : String
  }
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

type EditState
  = StaticState
  | EditState Response

type State a
  = Static String
  | Edit   String (Conflict.Form Form a) (Conflict.State a) (List String)

static : EditState
static = StaticState

toStatic : Form -> Form
toStatic form = { form | state = StaticState }

toEdit : Response -> Form -> Form
toEdit response form = { form | state = EditState response }

fromStateString : Maybe Response -> String -> Form -> Form
fromStateString response stateString =
  case stateString |> String.toLower of
    "static" -> toStatic
    "edit" ->
      case response of
        Just res -> toEdit res
        Nothing  -> identity

    _ -> identity

stateToString : Form -> String
stateToString form =
  case form.state of
    StaticState -> "static"
    EditState _ -> "edit"


done : HttpView.Migration Response -> Form -> Form
done mig =
  case mig |> HttpView.isSuccess of
    Just _  -> toStatic
    Nothing -> identity

compose : Maybe Response -> Init -> Form -> View
compose response model form =
  let
    res =
      ( case form.state of
        StaticState -> Nothing
        EditState r -> r |> HttpView.body |> Just

      , response |> Maybe.map HttpView.body
      )
    error = "conflict"
    view =
      { name     = form |> Conflict.init error res model.name
      , memo     = form |> Conflict.init error res model.memo
      , age      = form |> Conflict.init error res model.age
      , email    = form |> Conflict.init error res model.email
      , tel      = form |> Conflict.init error res model.tel
      , birthday = form |> Conflict.init error res model.birthday
      , start_at = form |> Conflict.init error res model.start_at
      , gender   = form |> Conflict.init error res model.gender
      , quality  = form |> Conflict.init error res model.quality
      , roles    = form |> Conflict.init error res model.roles
      }
    errors = List.concat
      [ view.name     |> Validate.errors
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
      { state = form.state
      , last  = response
      , hasError = errors |> List.isEmpty |> not
      }
      view


last : View -> Maybe Response
last (View info _) = info.last

isStatic : View -> Bool
isStatic (View info _) = info.state == StaticState

hasError : View -> Bool
hasError (View info _) = info.hasError


name     (View info form) = form.name     |> expose info.state
memo     (View info form) = form.memo     |> expose info.state
age      (View info form) = form.age      |> expose info.state
email    (View info form) = form.email    |> expose info.state
tel      (View info form) = form.tel      |> expose info.state
birthday (View info form) = form.birthday |> expose info.state
start_at (View info form) = form.start_at |> expose info.state
gender   (View info form) = form.gender   |> expose info.state
quality  (View info form) = form.quality  |> expose info.state
roles    (View info form) = form.roles    |> expose info.state

expose : EditState -> Validate.Model (Conflict.Form Form a) -> State a
expose st model =
  case model |> Validate.expose of
    (fieldName,form,errors) ->
      case st of
        StaticState -> Static fieldName
        EditState _ -> Edit   fieldName form ( form.field |> Conflict.state ) errors
