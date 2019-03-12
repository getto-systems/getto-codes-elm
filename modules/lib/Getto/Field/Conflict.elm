module Getto.Field.Conflict exposing
  ( Model
  , Field
  , Form
  , Prop
  , Init
  , State(..)
  , Resolve
  , init
  , form
  , expose
  , state
  , resolve
  , none
  , leave
  , revert
  )
import Getto.Field as Field
import Getto.Field.Form as Form
import Getto.Field.Validate as Validate

type Model response form a = Model (response -> a) (Validate.Model (Form form a))

type alias Field a = Field.Model (Attribute a) a
type alias Form form a = Form.Model form (Attribute a) a
type alias Prop form a = Form.Prop form (Attribute a) a

type Attribute a = Attribute
  { state     : State a
  , operation : Operation
  }

type State a
  = NoProblem
  | Conflict a

type Operation
  = None
  | OverWrite

type Resolve a
  = Leave
  | Revert a

type alias Init response form a = ( response -> a, Validate.Init form (Attribute a) a )

init : String -> ( Maybe response, response ) -> Init response form a -> form -> Model response form a
init error response (getter,initValidate) = Validate.init
  (\field ->
    let
      (Attribute attribute) = field |> Field.attribute

      newState =
        case response of
          ( Just first, last ) ->
            let
              firstValue = first |> getter
              lastValue  = last  |> getter

              formValue = field |> Field.value
            in
              case attribute.operation of
                OverWrite -> NoProblem
                None ->
                  if ( firstValue /= lastValue ) &&
                     ( firstValue /= formValue ) &&
                     ( formValue  /= lastValue )
                    then Conflict lastValue
                    else NoProblem

          _ -> NoProblem
    in
      ( field |> Field.setAttribute
        ( Attribute
          { state     = newState
          , operation = attribute.operation
          }
        )
      , case newState of
        NoProblem  -> []
        Conflict _ -> [error]
      )
  )
  initValidate
  >> Model getter

form : Model response form a -> Validate.Model (Form form a)
form (Model _ model) = model

expose : response -> Model response form a -> ( String, ( a, Form form a ), List String )
expose res (Model getter model) =
  let
    (name,validateForm,errors) = model |> Validate.expose
  in
    ( name
    , ( res |> getter, validateForm )
    , errors
    )

state : Field a -> State a
state = Field.attribute >> (\(Attribute attribute) -> attribute.state )

resolve : Prop form a -> Resolve a -> form -> form
resolve prop mig =
  Form.setAttribute prop
    ( Attribute
      { state     = NoProblem
      , operation = OverWrite
      }
    )
  >>
    ( case mig of
      Leave        -> identity
      Revert value -> Form.set prop value
    )

none : Attribute a
none = Attribute
  { state     = NoProblem
  , operation = None
  }

leave : Resolve a
leave = Leave

revert : a -> Resolve a
revert = Revert
