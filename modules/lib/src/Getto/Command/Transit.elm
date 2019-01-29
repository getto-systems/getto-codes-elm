module Getto.Command.Transit exposing
  ( none
  , map
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

none : model -> ( model, Cmd msg )
none model = ( model, Cmd.none )

map : (msg -> super) -> ( model, Cmd msg ) -> ( model, Cmd super )
map = Cmd.map >> Tuple.mapSecond

andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f (model,cmd) =
  let
    (newModel,newCmd) = model |> f
  in
    ( newModel, Cmd.batch [ cmd, newCmd ] )

compose : (a -> model) -> ( a, Cmd msg ) -> ( model, Cmd msg )
compose model (a,msgA) = ( model a, msgA )

compose2 : (a -> b -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( model, Cmd msg )
compose2 model (a,msgA) (b,msgB) = ( model a b, [msgA,msgB] |> Cmd.batch )

compose3 : (a -> b -> c -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( model, Cmd msg )
compose3 model (a,msgA) (b,msgB) (c,msgC) = ( model a b c, [msgA,msgB,msgC] |> Cmd.batch )

compose4 : (a -> b -> c -> d -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( d, Cmd msg ) -> ( model, Cmd msg )
compose4 model (a,msgA) (b,msgB) (c,msgC) (d,msgD) = ( model a b c d, [msgA,msgB,msgC,msgD] |> Cmd.batch )

compose5 : (a -> b -> c -> d -> e -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( d, Cmd msg ) -> ( e, Cmd msg ) -> ( model, Cmd msg )
compose5 model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) = ( model a b c d e, [msgA,msgB,msgC,msgD,msgE] |> Cmd.batch )

compose6 : (a -> b -> c -> d -> e -> f -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( d, Cmd msg ) -> ( e, Cmd msg ) -> ( f, Cmd msg ) -> ( model, Cmd msg )
compose6 model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) = ( model a b c d e f, [msgA,msgB,msgC,msgD,msgE,msgF] |> Cmd.batch )

compose7 : (a -> b -> c -> d -> e -> f -> g -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( d, Cmd msg ) -> ( e, Cmd msg ) -> ( f, Cmd msg ) -> ( g, Cmd msg ) -> ( model, Cmd msg )
compose7 model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) (g,msgG) = ( model a b c d e f g, [msgA,msgB,msgC,msgD,msgE,msgF,msgG] |> Cmd.batch )

compose8 : (a -> b -> c -> d -> e -> f -> g -> h -> model) -> ( a, Cmd msg ) -> ( b, Cmd msg ) -> ( c, Cmd msg ) -> ( d, Cmd msg ) -> ( e, Cmd msg ) -> ( f, Cmd msg ) -> ( g, Cmd msg ) -> ( h, Cmd msg ) -> ( model, Cmd msg )
compose8 model (a,msgA) (b,msgB) (c,msgC) (d,msgD) (e,msgE) (f,msgF) (g,msgG) (h,msgH) = ( model a b c d e f g h, [msgA,msgB,msgC,msgD,msgE,msgF,msgG,msgH] |> Cmd.batch )
