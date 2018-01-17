" ::::::::: Cursor ::::::::::::::::::::::::

" ··········· helpers ········· {{{1
" Do I still need/want this?
highlight! CursorLineClear guibg=NONE guifg=NONE gui=NONE ctermbg=NONE ctermfg=NONE cterm=NONE

func! InitCursorVars()
  if !exists("w:cursorline_on")
    let w:cursorline_on = 0
  endif
endf

func! RestoreCrsr()
  if line("'\"") > 1 && line("'\"") <= line("$")
    exe "normal! g`\""
  endif
endf
" ····························· }}}1

" Gui cursor
set guicursor=n-v-c:block-blinkon0
set guicursor+=ve:ver35
set guicursor+=o:hor50
set guicursor+=i-ci:ver25
set guicursor+=r-cr:hor20
set guicursor+=sm:block-blinkon0

" cursor shape
if has("nvim")
  " it all just works!
elseif exists("$ITERM_PROFILE")
  if &term =~ 'xterm'
    let &t_SI="\<Esc>[1337;CursorShape=1\x7"
    let &t_SR="\<Esc>[1337;CursorShape=2\x7"
    let &t_EI="\<Esc>[1337;CursorShape=0\x7"
  elseif &term =~ 'tmux\|screen' && exists("$TMUX")
    let &t_SI="\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_SR="\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
    let &t_EI="\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  endif
elseif &term =~ 'xterm'
  let &t_SI="\<Esc>[6 q"
  let &t_SR="\<Esc>[4 q"
  let &t_EI="\<Esc>[0 q"
endif

" cursorline
set cursorline
let w:cursorline_on = 0

augroup CursorGroup
  autocmd!
  autocmd BufReadPost            * call RestoreCrsr()
  autocmd BufReadPost,BufNewFile * call InitCursorVars()
augroup END
