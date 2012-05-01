"日本語の切り替え
set noimdisable
set noimdisableactivate
"フォント
set guifont=Ricty\ Regular:h13

"幅
set columns=150
"高さ
set lines=50

"カラースキーマ
set background=dark
colorscheme solarized
"文字色を調整
hi Normal guifg=#E7E7E7
"現在行に色をつける
set cursorline
set cursorcolumn

"タブ
set expandtab
set tabstop=2


"半透明
set transparency=4

"VimFiler
let g:vimfiler_as_default_explorer=1
let g:vimfiler_edit_action="tabopen"
let g:vimfiler_safe_mode_by_default=0


"改行のあれ
set formatoptions-=or

"foreman start
nnoremap <F7> :!foreman start<CR>
