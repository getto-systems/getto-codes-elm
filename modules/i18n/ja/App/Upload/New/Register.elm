module GettoUpload.I18n.App.Upload.New.Register exposing
  ( title
  , field
  , error
  )

title : String -> String
title name =
  case name of
    "register" -> "登録"

    _ -> name

field : String -> String
field name =
  case name of
    "name" -> "名前"
    "text" -> "テキスト"

    _ -> name

error : String -> String
error name =
  case name of
    "blank"   -> "入力してください"
    "no-file" -> "ファイルを選択してください"

    _ -> name
