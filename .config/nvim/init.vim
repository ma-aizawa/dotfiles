if &compatible
  set nocompatible
endif

if has('g:nyaovim_version')
  let s:dein_cache_path = expand('~/.cache/nyaovim/dein')
elseif has('nvim')
  let s:dein_cache_path = expand('~/.cache/nvim/dein')
else
  let s:dein_cache_path = expand('~/.cache/vim/dein')
endif

let s:dein_dir = s:dein_cache_path . '/repos/github.com/Shougo/dein.vim'

if &runtimepath != '/dein.vim'
  if !isdirectory(s:dein_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_dir
  endif
  execute 'set runtimepath+=' . fnamemodify(s:dein_dir, ':p')
endif

if dein#load_state(s:dein_cache_path)
  call dein#begin(s:dein_cache_path)

  call dein#load_toml('~/.config/nvim/dein.toml', {'lazy' : 0})
  call dein#load_toml('~/.config/nvim/deinlazy.toml', {'lazy' : 1})

  if exists('g:nyaovim_version')
    call dein#add('rhysd/nyaovim-markdown-preview')
  endif

  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
endif

" Vim pluginのhelp fileを生成
" https://github.com/w0rp/ale#generating-vim-help-files
packloadall
silent! helptag ALL


" 基本的なキーマッピングや設定 {{{
syntax enable
set number
set ruler
set showcmd
set autoread
set cursorline
set cursorcolumn

set enc=utf8
set fenc=utf8
set fencs=utf8,iso-2022-jp,euc-jp,cp932

set list
set listchars=eol:$,tab:>;,extends:<
set backspace=indent,eol,start
set wildchar=<Tab>
set showmatch

set shiftwidth=2
set tabstop=2
set softtabstop=2
set nosmarttab
set nojoinspaces
set noincsearch
set expandtab
set autoindent
set textwidth=0

set spelllang=enc,cjk
set nf=alpha

set splitbelow
set splitright

filetype on
filetype indent on
filetype plugin on
" }}}

" 行頭のコメント引き継ぎをやめる {{{
augroup HEADCOMMENT
  autocmd!
  autocmd FileType * set formatoptions-=o
  autocmd FileType * set formatoptions-=r
augroup END
" }}}

" ショートカット系のコマンド {{{
nnoremap gn :<C-u>tabnext<CR>
nnoremap gp :<C-u>tabprevious<CR>
nnoremap <Enter> :<C-u>w<CR>

nnoremap <Leader>t :<C-u>tabnew<CR>

nnoremap [nvim] <Nop>
nmap <Leader>v [nvim]

nnoremap [nvim]r :<C-u>source ~/.config/nvim/init.vim<CR>
nnoremap [nvim]v :<C-u>e ~/.config/nvim/init.vim<CR>
nnoremap [nvim]<Space> :<C-u>vnew ~/.config/nvim/init.vim<CR>
nnoremap [nvim]f :<C-u>vnew ~/.config/nvim/dein.toml<CR>
nnoremap [nvim]g :<C-u>vnew ~/.config/nvim/deinlazy.toml<CR>
nnoremap [nvim]d :<C-u>vnew ~/.config/nvim/
nnoremap [nvim]a :<C-u>vnew ~/.config/nvim/plugins/

" ターミナルモードでノーマルモードにするためのコマンド
tnoremap <silent> <ESC> <C-\><C-n>
nnoremap <Leader>g :<C-u>vnew <CR>:terminal<CR>GAsource ~/.bash_profile<CR>
nnoremap <Leader>eg :<C-u>:terminal<CR>GAsource ~/.bash_profile<CR>

" 個人的に便利だと思うVimの基本設定ラインキングより {{{
" http://itchyny.hatenablog.com/entry/2014/12/25/090000
nnoremap Y y
set display=lastline
set pumheight=10
set matchtime=1
nnoremap + <C-a>
nnoremap - <C-x>
augroup swapchoice-readonly
  autocmd!
  autocmd SwapExists * let v:swapchoice = 'o'
augroup END
" }}}
" }}}

source ~/.config/nvim/myscript.vim

