module Getto.Apply exposing
  ( apply
  , apply2
  , apply3
  , apply4
  , apply5
  , apply6
  , apply7
  , apply8
  , apply9
  , apply10
  , apply11
  , apply12
  , apply13
  , apply14
  , apply15
  , apply16
  )

apply : (a -> model) -> (init -> a) -> init -> model
apply construct a model = construct
  (model |> a)

apply2 : (a -> b -> model) -> (init -> a) -> (init -> b) -> init -> model
apply2 construct a b model = construct
  (model |> a)
  (model |> b)

apply3 : (a -> b -> c -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> init -> model
apply3 construct a b c model = construct
  (model |> a)
  (model |> b)
  (model |> c)

apply4 : (a -> b -> c -> d -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> init -> model
apply4 construct a b c d model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)

apply5 : (a -> b -> c -> d -> e -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> init -> model
apply5 construct a b c d e model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)

apply6 : (a -> b -> c -> d -> e -> f -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> init -> model
apply6 construct a b c d e f model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)

apply7 : (a -> b -> c -> d -> e -> f -> g -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> init -> model
apply7 construct a b c d e f g model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)

apply8 : (a -> b -> c -> d -> e -> f -> g -> h -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> init -> model
apply8 construct a b c d e f g h model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)

apply9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> init -> model
apply9 construct a b c d e f g h i model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)

apply10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> init -> model
apply10 construct a b c d e f g h i j model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)

apply11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> init -> model
apply11 construct a b c d e f g h i j k model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)

apply12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> (init -> l) -> init -> model
apply12 construct a b c d e f g h i j k l model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)
  (model |> l)

apply13 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> (init -> l) -> (init -> m) -> init -> model
apply13 construct a b c d e f g h i j k l m model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)
  (model |> l)
  (model |> m)

apply14 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> (init -> l) -> (init -> m) -> (init -> n) -> init -> model
apply14 construct a b c d e f g h i j k l m n model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)
  (model |> l)
  (model |> m)
  (model |> n)

apply15 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> (init -> l) -> (init -> m) -> (init -> n) -> (init -> o) -> init -> model
apply15 construct a b c d e f g h i j k l m n o model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)
  (model |> l)
  (model |> m)
  (model |> n)
  (model |> o)

apply16 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> model) -> (init -> a) -> (init -> b) -> (init -> c) -> (init -> d) -> (init -> e) -> (init -> f) -> (init -> g) -> (init -> h) -> (init -> i) -> (init -> j) -> (init -> k) -> (init -> l) -> (init -> m) -> (init -> n) -> (init -> o) -> (init -> p) -> init -> model
apply16 construct a b c d e f g h i j k l m n o p model = construct
  (model |> a)
  (model |> b)
  (model |> c)
  (model |> d)
  (model |> e)
  (model |> f)
  (model |> g)
  (model |> h)
  (model |> i)
  (model |> j)
  (model |> k)
  (model |> l)
  (model |> m)
  (model |> n)
  (model |> o)
  (model |> p)
