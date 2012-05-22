"日本語の切り替え
set noimdisable

"Mac用とWindows用で分岐 {{{
if has('gui_macvim')
  set noimdisableactivate
  "半透明
  set transparency=4
else
  set guioptions=acefirR
  set transparency=240
endif
"Mac用とWindows用で分岐 }}}

"フォント
set guifont=Ricty\ Regular:h13

"カラースキーマ
"solarizedを読み込んだ後にアレンジ {{{
set background=dark
colorscheme desert
colorscheme solarized

"solarizedがインストールされていない場合はdesertを採用
if g:colors_name == 'solarized'
  "通常の文字を少し明るく
  hi Normal guifg=#E7E7E7
  "行番号を少し明るく
  hi LineNr guifg=#aaaaaa 
  "コメントを少し明るく
  hi Comment guifg=#b4b4b4 gui=underline
  hi vimLineComment guifg=#b4b4b4 gui=underline
  "現在行の設定
  hi CursorLine guibg=#402305
  "現在列の設定
  hi CursorColumn guibg=#094856
endif
"}}}

"gui settings {{{

"幅
set columns=150
"高さ
set lines=50
"現在行に色をつける
set cursorline
set cursorcolumn
"gui settings }}}

