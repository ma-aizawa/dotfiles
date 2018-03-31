" neocomplete / neosnippets {{{
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_target)

smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

let g:neocomplete#acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_ignore_case = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyward_length = 4
if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif

"Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory = '~/.config/nvim/snippets'

if !exists('g:neocomplete#source#omni#input_patterns')
  let g:neocomplete#source#omni#input_patterns = {}
endif


function! JumpNeosnippetDir()
  execute "vnew " . g:neosnippet#snippet_directory
endfunction
command! JumpNeosnippetDir :call JumpNeosnippetDir()
nnoremap [vimrc]<C-k> :<C-u>JumpNeosnippetDir<CR>

augroup AfterSnippet
  autocmd!
  autocmd BufWritePost *.snip,*.snippets call neosnippet#variables#set_snippets({})
augroup END

augroup SnippetFileType
  autocmd!
  autocmd BufEnter *_spec.rb NeoSnippetSource ~/.config/nvim/snippets/ruby.rspec.snip
augroup END
