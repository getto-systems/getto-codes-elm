module Getto.Prop exposing
  ( Prop
  , Getter
  , Updater
  , create
  , get
  , set
  , update
  )

type Prop big small = Prop (Getter big small) (Updater big small)

type alias Getter  big small = big -> small
type alias Updater big small = (small -> small) -> big -> big

create : Getter big small -> Updater big small -> Prop big small
create = Prop

get : Prop big small -> big -> small
get (Prop getter _) = getter

set : Prop big small -> small -> big -> big
set prop = always >> update prop

update : Prop big small -> (small -> small) -> big -> big
update (Prop _ updater) = updater
