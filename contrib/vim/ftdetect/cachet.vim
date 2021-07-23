" vim: set tw=99 ts=2 sts=2 sw=2 et:

autocmd BufRead,BufNewFile *.cachet call s:set_cachet_filetype()

function! s:set_cachet_filetype() abort
  if &filetype !=# 'cachet'
    set filetype=cachet
  endif
endfunction
