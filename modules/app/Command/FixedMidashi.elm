port module GettoCodes.Command.FixedMidashi exposing
  ( create
  )

port fixedMidashi : () -> Cmd msg

create : Cmd annonymous
create = fixedMidashi ()
