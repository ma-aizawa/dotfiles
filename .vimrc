"neobundle {{{

"NeoBundle 準備
set nocompatible
filetype off
filetype plugin indent off

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  call neobundle#rc(expand('~/.vim/bundle/'))
endif

"colorscheme
NeoBundle 'Solarized'

"vim-script by Shougo
"NeoBundle 'Shougo/vimproc'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'Shougo/neocomplcache'

"vim-scritps repo
NeoBundle 'vim-scripts/taglist.vim'

"programming
NeoBundle 'motemen/git-vim'
NeoBundle 'quickrun'
"Scala
NeoBundle 'vim-scala'
NeoBundle 'scala.vim'
"Ruby
NeoBundle 'rails.vim'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'endwise.vim'
"Util
NeoBundle 'calendar.vim'
"zencoding-vim
NeoBundle 'mattn/zencoding-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'mattn/gist-vim'

"project.vim
"http://www.vim.org/scripts/download_script.php?src_id=6273 project.vim
"最初に入れないとvim-scripts/project.vim.gitを入れてしまうので注意
NeoBundle 'project.vim' 

"neobundle }}}

"基本的な設定 {{{

"バックアップファイル系
set backupdir=~/.vim/backup
set directory=~/.vim/backup

syntax on
set number 
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
set noincsearch

set expandtab
set autoindent
set nocompatible
filetype on
filetype indent on
filetype plugin on

"行頭のコメントをやめる
set formatoptions-=o
set formatoptions-=r

"分割は右下に
set splitbelow
set splitright

"基本的な設定 }}}

"日本語入力 {{{
"日本語入力をリセット
au BufNewFile,BufRead * set iminsert=0

"日本語入力時のカーソル
if has('multi_byte_ime') || has('xim')
	highlight CursorIM guibg=Purple guifg=NONE
endif

"日本語入力 }}}

"折りたたみ {{{
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
"}}}

"vimrc編集時用の設定 {{{
augroup EditVim
autocmd!
"vimrcの時のみ折りたたみパターンを変更
autocmd FileType vim setlocal foldmethod=marker
autocmd FileType vim setlocal formatoptions-=ro
augroup END
"vimrc用 }}}

"編集用 {{{

"色をつける
augroup InsertHook
	autocmd!
	autocmd InsertEnter * highlight StatusLine guifg=#ddcc45 guibg=#2E4340
	autocmd InsertLeave * highlight StatusLine guifg=#2E4340 guibg=#ddcc45 
augroup END

"改行
nnoremap <Leader><Enter> o<ESC>

"編集用 }}}

"書いているコードの実行 {{{
"nnoremap <F8> :call RunProgram()<CR>

"scalaのコマンド
command! Scalac !scalac %
command! Scala !scala -classpath . %

function! RunProgram()
  let ext = expand("%:e")
  if ext == "rb"
    :w<CR>:!ruby %<CR>
  elseif ext == "scala"
    w
    !scalac %
    !scala -classpath . %<
  endif
endfunction

"書いているコードの実行 }}}

"git ショートカット {{{
"git-vimがあるので設定はほとんど不要

"git push heroku master
nnoremap <Leader>gh :GitPush heroku master<CR>

"git ショートカット }}}

"補完系設定 {{{
"eskk and neocomplcache
let g:gskk#enable_completion = 1

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
function! ClosePair(char)
  if getline('.')[col('.') - 1] == a:char
    return "\"
  else
    return a:char
  endif
endfunction

"補完系設定 }}}

"plugin setting {{{

"VimFiler {{{
let g:vimfiler_as_default_explorer=1
let g:vimfiler_edit_action="tabopen"
let g:vimfiler_safe_mode_by_default=0
"VimFiler }}}

"Gist-Vim {{{
let g:gist_open_browser_after_post = 1
"Gist-Vim }}}

"plugin }}}
