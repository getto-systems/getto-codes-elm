module Getto.Command.Transition exposing
  ( Update
  , none
  , batch
  , update
  , map
  , mapUpdate
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

type alias Update model msg = model -> ( model, Cmd msg )

none : Update model msg
none model = ( model, Cmd.none )

batch : List (Update model msg) -> Update model msg
batch list model = list |> List.foldl andThen ( model, Cmd.none )

update : (big -> small) -> (small -> big -> big) -> (small -> ( small, Update model msg )) -> (big -> ( big, Update model msg ))
update get set updateSmall model =
  model |> get |> updateSmall
  |> Tuple.mapFirst (\small -> model |> set small)

map : (msg -> super) -> ( model, Cmd msg ) -> ( model, Cmd super )
map = Cmd.map >> Tuple.mapSecond

mapUpdate : (msg -> super) -> ( model, Update m msg ) -> ( model, Update m super )
mapUpdate super = Tuple.mapSecond (\f -> f >> map super)

andThen : Update model msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f (model,cmd) =
  model |> f |> Tuple.mapSecond (\newCmd -> [cmd,newCmd] |> Cmd.batch)

compose : (List msg -> msg) -> (a -> model) -> ( a, msg ) -> ( model, msg )
compose batchMsg model (a,msgA) = ( model a, [msgA] |> batchMsg )

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
