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
