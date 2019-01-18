module Getto.Transit exposing
  ( none
  , batch
  )

none : model -> ( model, Cmd msg )
none model = ( model, Cmd.none )

batch : (model -> List (Cmd msg)) -> model -> ( model, Cmd msg )
batch cmds model = ( model, model |> cmds |> Cmd.batch )
