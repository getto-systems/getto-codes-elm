module GettoCodes.I18n.App.System.Profile exposing
  ( button
  )

button : String -> String
button name =
  case name of
    "logout" -> "ログアウト"

    _ -> name
