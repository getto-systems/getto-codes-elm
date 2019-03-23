module GettoCodes.I18n.Http exposing
  ( error
  )
import GettoCodes.View.Http as HttpView

error : HttpView.Error -> String
error err =
  case err of
    HttpView.BadUrl _             -> "アクセスエラー"
    HttpView.Timeout              -> "タイムアウトエラー"
    HttpView.NetworkError         -> "ネットワークエラー"
    HttpView.BadRequest           -> "リクエストエラー"
    HttpView.Unauthorized         -> "ログインしなおしてください"
    HttpView.Forbidden            -> "権限がありません"
    HttpView.NotFound             -> "存在しませんでした"
    HttpView.Conflict             -> "ほかの場所で変更されました"
    HttpView.UnprocessableEntity  -> "保存できませんでした"
    HttpView.PreconditionRequired -> "情報が送信されませんでした"
    HttpView.BadStatus _          -> "サーバーエラー"
    HttpView.BadResponse _        -> "レスポンスエラー"
