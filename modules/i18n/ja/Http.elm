module GettoUpload.I18n.Http exposing
  ( error
  )
import GettoUpload.View.Http as HttpView

error : HttpView.Error -> String
error err =
  case err of
    HttpView.BadUrl _            -> "アクセスエラー"
    HttpView.Timeout             -> "タイムアウトエラー"
    HttpView.NetworkError        -> "ネットワークエラー"
    HttpView.BadRequest          -> "リクエストエラー"
    HttpView.Unauthorized        -> "ログインしなおしてください"
    HttpView.Forbidden           -> "権限がありません"
    HttpView.NotFound            -> "存在しませんでした"
    HttpView.UnprocessableEntity -> "保存できませんでした"
    HttpView.BadStatus _         -> "サーバーエラー"
    HttpView.BadHeader e -> e |> Debug.log "bad-header" |> always "アプリケーションエラー"
    HttpView.BadBody   e -> e |> Debug.log "bad-body"   |> always "アプリケーションエラー"
