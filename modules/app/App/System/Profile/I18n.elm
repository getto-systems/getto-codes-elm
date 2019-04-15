module GettoCodes.App.System.Profile.I18n exposing
  ( button
  )

button : String -> String
button name =
  case name of
    "logout" -> "ログアウト"

    _ -> name
