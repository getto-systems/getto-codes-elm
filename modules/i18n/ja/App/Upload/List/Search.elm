module GettoUpload.I18n.App.Upload.List.Search exposing
  ( field
  , gender
  )

field : String -> String
field name =
  case name of
    "id"       -> "ID"
    "name"     -> "名前"
    "age"      -> "年齢"
    "email"    -> "メール"
    "tel"      -> "電話番号"
    "birthday" -> "誕生日"
    "start_at" -> "開始時間"
    "gender"   -> "性別"
    "roles"    -> "権限"

    _ -> name

gender : String -> String
gender name =
  case name of
    "male"   -> "男性"
    "female" -> "女性"
    "other"  -> "その他"

    _ -> name
