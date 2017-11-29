" ::::::::: Functions :::::::::::::::::::::

" ··········· folding ·················· {{{1
func! MyFoldText()
  let l:text =  '+'
  let l:text .= substitute(v:folddashes, '-', '·', 'g')
  let l:text .= substitute(getline(v:foldstart), '^\S', ' &', '')
  return l:text
endf

" ··········· syntax ··················· {{{1
func! SynStack()
  return map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endf

func! SynHighlights(...)
  let l:stack  = SynStack()
  let l:arg    = a:0 ? a:1 : 0
  let l:offset = max([l:arg, -len(l:stack)])

  exe 'echo expand("<cword>")'
  exe 'echo " "'

  for name in l:stack[l:offset:]
    exe 'verbose hi ' . name
  endfor
endf

command! -nargs=0 SynStack call SynStack()
command! -nargs=0 SynHighlight call SynHighlights(-1)
command! -nargs=? SynHighlights call SynHighlights(<args>)

" ··········· braces ··················· {{{1
func! NextTextObject(motion)
  echo
  let c = nr2char(getchar())
  execute "normal! f" . c . "v" . a:motion . c
endf

" ··········· tab key ·················· {{{1
func! SuperTab(complete, tab)
  " complete if popup-menu displayed
  if pumvisible() | return a:complete | endif

  let line = getline('.')  " current line
  let col  = col('.') - 2  " previous character's col index

  " tab if not finishing a word/filename
  if empty(line) || line[col] !~ '\k\|[/~.]' || line[col + 1] =~ '\k' || &expandtab == 0
    " on empty line
    " OR not following part of a word/filename
    " OR within a word/filename
    " OR using tabs, not spaces
    return a:tab
  endif

  " group of non-whitespace characters before cursor
  let prefix = expand(matchstr(line[0:col], '\S*$'), 1)

  " complete filename if finishing a path
  if prefix =~ '^[~/.]' | return "\<c-x>\<c-f>" | endif

  " perform custom completion if possible
  if !empty(&completefunc) && call(&completefunc, [1, prefix]) >= 0
    return "\<c-x>\<c-u>"
  endif

  return a:complete
endf

" ··········· expression helpers ······· {{{1
func! ExprSideEffect(side_effect)
  execute a:side_effect
  return ''
endf

" ··········· git ······················ {{{1
"Git branch
func! GitBranch()
  let branch = system("git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* //'")
  if branch != ''
    return '(' . substitute(branch, '\n', '', 'g') . ')'
  endif
  return ''
endf

func! ReloadBuffers()
  set autoread
  silent! checktime
  set noautoread
  echo 'reloaded!'
endf

" ··········· rspec ···················· {{{1
func! RSpecLet(bang)
  delete
  normal! mz

  call MoveAssignment()
  call ReplaceWithLet(a:bang)

  normal! `z
endf

func! MoveAssignment()
  call search('\v^\s*<let>|<context>|<describe>', 'b')
  put
  normal! ==
endf

func! ReplaceWithLet(bang)
  execute 'substitute/\v^\s+\zs(\w+)%(\s*\=\s*)(.*\S)\s*$/let'.a:bang.'(:\1) { \2 }'
endf

" ··········· keyboard ················· {{{1
func! Keyboard(type)
  if a:type == "workman"
    call NormanUnmaps()
    call QWERTYUnmaps()
    call WorkmanMaps()
    echo "Workman mappings enabled"
  elseif a:type == "norman"
    call QWERTYUnmaps()
    call WorkmanUnmaps()
    call NormanMaps()
    echo "Norman mappings enabled"
  else
    call NormanUnmaps()
    call WorkmanUnmaps()
    call QWERTYMaps()
    echo "QWERTY mappings enabled"
  endif
endf

" ··········· norman ··················· {{{2
func! NormanMaps()
  " ··· home ············ {{{3
  noremap <C-Y> h
  noremap <C-I> k
  noremap <C-O> l
  noremap <C-H> <C-I>
  noremap <C-J> <C-Y>
  noremap <C-P> <C-O>
  " }}}
  " ··· arch ············ {{{3
  " noremap <C-R> k
  " noremap <C-Y> h
  " noremap <C-L> l
  " noremap <C-U> <C-R>
  " noremap <C-J> <C-Y>
  " noremap     l <C-L>
  " }}}
endf
func! NormanUnmaps()
  " ··· home ············ {{{3
  silent! unmap <C-Y>
  silent! unmap <C-I>
  silent! unmap <C-O>
  silent! unmap <C-H>
  silent! unmap <C-J>
  silent! unmap <C-P>
  " }}}
  " ··· arch ············ {{{3
  " silent! unmap <C-R>
  " silent! unmap <C-Y>
  " silent! unmap <C-L>
  " silent! unmap <C-U>
  " silent! unmap <C-J>
  " silent! unmap l
  " }}}
endf

" ··········· qwerty ··················· {{{2
func! QWERTYMaps()
endf
func! QWERTYUnmaps()
endf

" ··········· workman ·················· {{{2
func! WorkmanMaps()
  " ··· home ············ {{{3
  noremap <C-E> k
  noremap <C-Y> h
  noremap <C-O> l
  noremap k <C-E>
  noremap j <C-Y>
  noremap <C-K> <C-O>
  " }}}
  " ··· arch ············ {{{3
  " noremap <C-U> k
  " noremap <C-Y> h
  " noremap <C-P> l
  " noremap <C-J> <C-Y>
  " }}}
  " ··· swap ············ {{{3
  " noremap j k
  " noremap k j
  " }}}
  " ··· mix ············· {{{3
  " noremap <C-E> k
  " noremap <C-K> <C-E>
  " }}}
endf
func! WorkmanUnmaps()
  " ··· home ············ {{{3
  silent! unmap <C-E>
  silent! unmap <C-Y>
  silent! unmap <C-O>
  silent! unmap k
  silent! unmap j
  silent! unmap <C-K>
  " }}}
  " ··· arch ············ {{{3
  " silent! unmap <C-U>
  " silent! unmap <C-Y>
  " silent! unmap <C-P>
  " silent! unmap <C-J>
  " }}}
  " ··· swap ············ {{{3
  " silent! unmap j
  " silent! unmap k
  " }}}
  " ··· mix ············· {{{3
  " silent! unmap <C-E>
  " silent! unmap <C-K>
  " }}}
endf

" ··········· initialize ··············· {{{2
call QWERTYMaps()
" call WorkmanMaps()
