# aozora
emacsで青空文庫の縦書き表示

## 使い方

### aozora-view-mode
`M-x aozora-view` 青空文庫からダウンロードしたzipファイルを指定する

`n` -- 次のページへ移動  
`p` -- 前のページへ移動  
`B` -- その位置をブックマーク  
`G` -- ブックマークした位置へジャンプ  
`C` -- キャッシュを更新  
`V` -- 元の青空文庫形式のテキストファイルを開く

ファイル読み込み後に`$HOME/.aozora`ディレクトリを作られて  
展開したzipファイル、キャッシュ、ブックマークに使う

### aozora-list-mode
`M-x aozora-list` aozora-viewで読み込んだファイルの一覧を表示する

`n` -- カーソルを下へ移動  
`p` -- カーソルを上へ移動  
`RET` -- そのファイルを表示  
`s` -- バフッファの更新

## ライセンス
[MIT](https://github.com/cxxxr/aozora/blob/master/LICENSE)
