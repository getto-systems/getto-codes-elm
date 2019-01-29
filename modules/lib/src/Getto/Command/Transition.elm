module Getto.Command.Transition exposing
  ( Command
  , empty
  , none
  , exec
  , batch
  , update
  , map
  , mapCommand
  , andThen
  , compose
  , compose2
  , compose3
  , compose4
  , compose5
  , compose6
  , compose7
  , compose8
  )

type alias Command model msg = model -> Cmd msg

empty : model -> ( model, Cmd msg )
empty = exec none

none : Command model msg
none = always Cmd.none

exec : Command model msg -> model -> ( model, Cmd msg )
exec f model = ( model, model |> f )

batch : List (Command model msg) -> Command model msg
batch list model =
  list
  |> List.map (\f -> model |> f)
  |> Cmd.batch

update : (big -> small) -> (small -> big -> big) -> (small -> ( small, msg )) -> big -> ( big, msg )
update get set updateSmall model =
  model |> get |> updateSmall
  |> Tuple.mapFirst (\small -> model |> set small)

map : (msg -> super) -> ( model, Cmd msg ) -> ( model, Cmd super )
map = Cmd.map >> Tuple.mapSecond

mapCommand : (msg -> super) -> ( model, Command m msg ) -> ( model, Command m super )
mapCommand super = Tuple.mapSecond (\f -> f >> Cmd.map super)

andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f (model,cmd) =
  model |> f |> Tuple.mapSecond (\newCmd -> [cmd,newCmd] |> Cmd.batch)

compose : (List msg -> msg) -> (a -> model) -> ( a, msg ) -> ( model, msg )
compose batchMsg model (a,msgA) = ( model a, msgA )

compose2 : (List msg -> msg) -> (a -> b -> model) -> ( a, msg ) -> ( b, msg ) -> ( model, msg )
compose2 batchMsg model (a,msgA) (b,msgB) = ( model a b, [msgA,msgB] |> batchMsg )

compose3 : (List msg -> msg) -> (a -> b -> c -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( model, msg )
compose3 batchMsg model (a,msgA) (b,msgB) (c,msgC) = ( model a b c, [msgA,msgB,msgC] |> batchMsg )

compose4 : (List msg -> msg) -> (a -> b -> c -> d -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( d, msg ) -> ( model, msg )
compose4 batchMsg model (a,msgA) (b,msgB) (c,msgC) (d,msgD) = ( model a b c d, [msgA,msgB,msgC,msgD] |> batchMsg )

compose5 : (List msg -> msg) -> (a -> b -> c -> d -> e -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( d, msg ) -> ( e, msg ) -> ( model, msg )
compose5 batchMsg model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) = ( model a b c d e, [msgA,msgB,msgC,msgD,msgE] |> batchMsg )

compose6 : (List msg -> msg) -> (a -> b -> c -> d -> e -> f -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( d, msg ) -> ( e, msg ) -> ( f, msg ) -> ( model, msg )
compose6 batchMsg model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) = ( model a b c d e f, [msgA,msgB,msgC,msgD,msgE,msgF] |> batchMsg )

compose7 : (List msg -> msg) -> (a -> b -> c -> d -> e -> f -> g -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( d, msg ) -> ( e, msg ) -> ( f, msg ) -> ( g, msg ) -> ( model, msg )
compose7 batchMsg model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) (g,msgG) = ( model a b c d e f g, [msgA,msgB,msgC,msgD,msgE,msgF,msgG] |> batchMsg )

compose8 : (List msg -> msg) -> (a -> b -> c -> d -> e -> f -> g -> h -> model) -> ( a, msg ) -> ( b, msg ) -> ( c, msg ) -> ( d, msg ) -> ( e, msg ) -> ( f, msg ) -> ( g, msg ) -> ( h, msg ) -> ( model, msg )
compose8 batchMsg model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) (g,msgG) (h,msgH) = ( model a b c d e f g h, [msgA,msgB,msgC,msgD,msgE,msgF,msgG,msgH] |> batchMsg )
