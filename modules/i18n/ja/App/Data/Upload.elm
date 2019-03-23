module GettoCodes.I18n.App.Data.Upload exposing
  ( title
  , field
  , error
  , message
  , button
  , gender
  , quality
  , state
  )
import GettoCodes.App.Data.Upload.Edit.Data.View as Data
import GettoCodes.I18n.App as AppI18n

title : String -> String
title name =
  case name of
    "detail"   -> "詳細情報"
    "complete" -> "状態"

    "to-complete" -> "完了します"
    "to-work"     -> "作業中に戻します"

    _ -> AppI18n.box name

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
    "state"    -> "状態"

    _ -> name

error : String -> String
error name =
  case name of
    _ -> name |> AppI18n.error

message : String -> String
message name =
  case name of
    "to-complete" -> "作業を完了して内容を編集できないようにします"
    "to-work"     -> "作業中に戻して内容を編集できるようにします"

    "unregister" -> "この項目を完全に削除します"

    _ -> name

button : String -> String
button name =
  case name of
    "to-complete" -> "完了する"
    "to-work"     -> "作業中に戻す"

    "to-complete-connecting" -> "完了しています"
    "to-work-connecting"     -> "作業中に戻しています"

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

state : Data.State -> String
state st =
  case st of
    Data.Working  -> "作業中"
    Data.Complete -> "完了"
