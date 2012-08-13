"日本語の切り替え
set noimdisable

"OSで分岐 {{{
"Mac
if has('gui_macvim')
  set noimdisableactivate
  "半透明
  set transparency=4
  "フォント
  set guifont=Ricty\ Regular:h13
"Windows
elseif has('win64')
  set guioptions=acefirR
  set transparency=240
  "フォント
  set guifont=Ricty\ Regular:h13
"Linux
else
  set guifont=Ricty-10.5
endif
"Mac用とWindows用で分岐 }}}

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
  "コメントを少し明るく・下線にする
  hi Comment guifg=#b4b4b4 gui=underline
  hi vimLineComment guifg=#b4b4b4 gui=underline
  "現在行の設定
  hi CursorLine guibg=#402305
  "現在列の設定
  hi CursorColumn guibg=#0948a6
endif
"}}}

"gui settings {{{

"幅
set columns=180
"高さ
set lines=62
"現在行に色をつける
set cursorline
set cursorcolumn

" from http://d.hatena.ne.jp/kasahi/20070902/1188744907
highlight WhitespaceEOL ctermbg=red guibg=red
augroup WhiteSpaceEOL
  autocmd!
  autocmd WinEnter * match WhitespaceEOL /\s\+$/
augroup END

"折り返しオフ
set textwidth=0

"InsertModeに入ったときにステータスバーの色を変える
augroup InsertColorChange
  autocmd!
  autocmd InsertEnter * hi StatusLine guibg=#ffff66 guifg=#ff6633
  autocmd InsertLeave * hi StatusLine guibg=#073642 guifg=#839496
augroup END

"gui settings }}}

" when open vim, default nerdtree open.
let NEADTreeShowHidden = 1
let file_name = expand("%:p")
if has('vim_starting') && file_name == ""
  autocmd VimEnter * execute 'NERDTree ./'
endif

