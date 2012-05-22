"日本語の切り替え
set noimdisable

"Mac用とWindows用で分岐
if has('gui_macvim')
  set noimdisableactivate
  "半透明
  set transparency=4
else
  set guioptions=acefirR
endif

"フォント
set guifont=Ricty\ Regular:h13

"幅
set columns=150
"高さ
set lines=50

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
  hi vimLineComment links to Comment
  "現在行の設定
  hi CursorLine guibg=#402305
  "現在列の設定
  hi CursorColumn guibg=#094856
endif
"}}}

"現在行に色をつける
set cursorline
set cursorcolumn

"タブ
set expandtab
set tabstop=2


"VimFiler
let g:vimfiler_as_default_explorer=1
let g:vimfiler_edit_action="tabopen"
let g:vimfiler_safe_mode_by_default=0


"改行のあれ
set formatoptions-=or

"foreman start
nnoremap <F7> :!foreman start<CR>
