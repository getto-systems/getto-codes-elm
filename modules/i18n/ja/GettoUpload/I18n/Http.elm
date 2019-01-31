module GettoUpload.I18n.Http exposing
  ( error
  )
import GettoUpload.Layout.View.Http as HttpView

error : HttpView.Error -> String
error err =
  case err of
    HttpView.BadUrl     _ -> "アクセスエラー"
    HttpView.Timeout      -> "タイムアウトエラー"
    HttpView.NetworkError -> "ネットワークエラー"
    HttpView.BadBody    _ -> "アプリケーションエラー"
    HttpView.BadStatus status ->
                    case status of
                      400 -> "リクエストエラー"
                      401 -> "ログインしなおしてください"
                      403 -> "権限がありません"
                      404 -> "存在しませんでした"
                      422 -> "保存できませんでした"
                      _   -> "サーバーエラー"
