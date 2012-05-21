"Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()


"colorscheme
Bundle 'Solarized'

Bundle 'unite.vim'
"Bundle 'L9' 何のプラグインか忘れた
Bundle 'caw'
Bundle 'echodoc'
Bundle 'eskk.vim'
"Bundle 'fuzzyfinder' 何のプラグインか忘れた
Bundle 'git-vim'
Bundle 'neocomplcache'
Bundle 'endwise.vim'
"projectは必須だけどBundleできず
Bundle 'project' 
Bundle 'quickrun'
Bundle 'solarized'
"Bundle 'surround' 何のプラグインか忘れた
"taglistは出来れば欲しいけどBundleできず
Bundle 'taglist'
Bundle 'vim-ruby'
Bundle 'vim-smartchr'
Bundle 'vimfiler'
Bundle 'vimproc'
Bundle 'vimshell'
"Scala
Bundle 'vim-scala'
Bundle 'scala.vim'
"rails
Bundle 'rails.vim'
"便利系
Bundle 'calendar.vim'

filetype plugin indent on

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
set nojoinspaces

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

"書いているコードの実行
nnoremap <F8> :call RunProgram()<CR>

function RunProgram()
  let ext = expand("%:e")
  if ext == "rb"
    :w<CR>:!ruby %<CR>
  elseif ext == "scala"
    w
    !scalac %
    !scala -classpath . %<
  endif
endfunction

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

"scalaのコマンド
command! Scalac !scalac %
command! Scala !scala -classpath . %

"補完
let g:neocomplcache_enable_at_startup = 1

"大文字が入力されるまで大文字小文字の区別を無視する
let g:neocomplcache_enable_smart_case = 1

"区切りの補完を有効化
let g:neocomplcache_enable_underbar_completion = 1

"シンタックスをキャッシュする時の最小文字長を3
let g:neocomplcache_min_syntax_length = 4

"Enable heavy omni completion. ruby用
if !exists('g:neocomplcache_omni_patterns')
  let g:neocomplcache_omni_patterns = {}
endif
let g:neocomplcache_omni_patterns.ruby = '[^.*\t]\.\w*\|\h\w*::'
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

"補完候補が表示されている場合は確定。そうでない場合は改行
"inoremap pumvisible() ? neocomplcache#close_popup() : "\"

" pair close checker.
" from othree vimrc ( http://github.com/othree/rc/blob/master/osx/.vimrc )
function ClosePair(char)
  if getline('.')[col('.') - 1] == a:char
    return "\"
  else
    return a:char
  endif
endfunction
