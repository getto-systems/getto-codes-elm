module GettoUpload.I18n.App.Upload.New.Register exposing
  ( title
  , field
  , error
  , gender
  , quality
  )

title : String -> String
title name =
  case name of
    "register" -> "登録"

    _ -> name

field : String -> String
field name =
  case name of
    "name"     -> "名前"
    "text"     -> "テキスト"
    "memo"     -> "備考"
    "age"      -> "年齢"
    "email"    -> "メール"
    "tel"      -> "電話番号"
    "birthday" -> "誕生日"
    "start_at" -> "開始時間"
    "gender"   -> "性別"
    "quality"  -> "品質"
    "roles"    -> "権限"

    _ -> name

error : String -> String
error name =
  case name of
    "blank"   -> "入力してください"
    "no-file" -> "ファイルを選択してください"

    _ -> name

gender : String -> String
gender name =
  case name of
    "male"   -> "男性"
    "female" -> "女性"
    "other"  -> "その他"

    _ -> name

quality : String -> String
quality name =
  case name of
    "high" -> "高品質"
    "low"  -> "低品質"

    _ -> name
