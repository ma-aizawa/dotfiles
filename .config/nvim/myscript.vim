function! SaveCursor()
  let g:before_line = line('.')
  let g:before_column = col('.')
endfunction
function! RemoveTailWhiteSpaces()
  if &filetype == 'markdown'
    return
  else
    silent! %s/\s\+$//g
  endif
endfunction
function! RestoreCursor()
  silent! execute "call cursor(" . g:before_line . "," . g:before_column . ")"
endfunction

function! FixSpellMiss()
  if &filetype == 'vim'
    " このファイルをfixしないための設定
    return
  endif
  silent! %s/TIme/Time/g
  silent! %s/assing/assign/g
  silent! %s/caceh/cache/g
  silent! %s/clinet/client/g
  silent! %s/calss/class/g
  silent! %s/errro/error/g
endfunction

augroup WhenSave
  autocmd!
  autocmd BufWritePre * :call SaveCursor()
  autocmd BufWritePre * :call FixSpellMiss()
  autocmd BufWritePre * :call RemoveTailWhiteSpaces()
  autocmd BufWritePost <silent> redraw
  autocmd BufWritePost * :call RestoreCursor()
augroup END

function! CopyPath()
  let @*=expand('%:p')
  let @"=expand('%:p')
endfunction
command! CP :call CopyPath()

