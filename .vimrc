"バックアップファイル系
set backupdir=~/.vim/backup
set directory=~/.vim/backup

"シンタックス
syntax on
"行番号の表示
set number 
"インデント
set list
set listchars=eol:$,tab:>;,extends:<
set shiftwidth=2
set tabstop=2
set enc=utf8
set fenc=utf8
set fencs=utf8,iso-2022-p,enc-jp,cp932
set backspace=indent,eol,start
set wildchar=<Tab>
set showmatch
set nosmarttab

set expandtab
set autoindent
set nocompatible
filetype on
filetype indent on
filetype plugin on

"色をつける
augroup InsertHook
	autocmd!
	autocmd InsertEnter * highlight StatusLine guifg=#ddcc45 guibg=#2E4340
	autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ddcc45 
augroup END

"日本語入力をリセット
au BufNewFile,BufRead * set iminsert=0

"日本語入力時のカーソル
if has('multi_byte_ime') || has('xim')
	highlight CursorIM guibg=Purple guifg=NONE
endif

"pathogen プラグイン管理
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
set helpfile=$VIMRUNTIME/doc/help.txt

filetype on

"eskk and neocomplcache
let g:gskk#enable_completion = 1

"インクリメントサーチオフ
set noincsearch

"行頭のコメントをやめる
set formatoptions-=o
set formatoptions-=r

"分割は右下に
set splitbelow
set splitright

"rubyの実行
nnoremap <F8> :w<CR>:!ruby %<CR>

"折りたたみ
set foldmethod=syntax

"rubyの折りたたみ設定をまねしてみる 
set foldlevel=1
set foldnestmax=2

augroup foldmethod-syntax
	autocmd!
	autocmd InsertEnter * if &l:foldmethod ==# 'syntax'
	\                   |   setlocal foldmethod=manual
	\                   | endif
	autocmd InsertLeave * if &l:foldmethod ==# 'manual'
	\                   |   setlocal foldmethod=syntax
	\                   | endif
augroup END

"改行
nnoremap <Leader><Enter> o<ESC>

"git push heroku master
nnoremap <Leader>gh :GitPush heroku master<CR>

