" ::::::::: Keybindings :::::::::::::::::::

" ··········· config ·········· {{{1
" Use Space as mapleader
map <Space> <Leader>

" Timeout mappings fast and key codes faster
set timeout timeoutlen=1000 ttimeoutlen=5

" source / edit vimrc
nnoremap <silent> <leader>f= :DotVimReload<CR>
nnoremap          <leader>f; :DotVim init/
nnoremap       <leader>f<CR> :DotVim 

" ··········· buffers ········· {{{1
nmap     <leader>o     :edit <C-R>=fnameescape(expand('%:~:.:h')).'/'<CR>
nnoremap <leader>e     :edit 
nnoremap <leader><C-E> :edit **/

nnoremap <leader>fs    :write<CR>
nnoremap <leader>wq    :wq 

nnoremap  <leader>dd   :bdelete<CR>
nnoremap  <leader>dl   :bdelete#<CR>

" Buffer list
nnoremap <leader>bn    :bnext<CR>
nnoremap <leader>bp    :bprevious<CR>
nnoremap <leader>l     <C-^>

" Quickfix list (alt h/j/k/l)
nmap     <C-@>         <Plug>(listical_quickfix)
nmap     <C-Space>     <Plug>(listical_quickfix)
nnoremap <M-j>         :cnext<CR>
nnoremap <M-k>         :cprevious<CR>
nnoremap <M-h>         :colder<CR>
nnoremap <M-l>         :cnewer<CR>

" Location List (alt H/J/K/L)
nmap     <C-S-Space>   <Plug>(listical_loclist)
nnoremap <M-J>         :lnext<CR>
nnoremap <M-K>         :lprevious<CR>
nnoremap <M-H>         :lolder<CR>
nnoremap <M-L>         :lnewer<CR>

" ··········· command-line ···· {{{1
noremap  <leader>x     :
nnoremap <leader>1     :!
nnoremap <leader>h     :help 
nnoremap <leader><C-H> :help <C-R><C-W>

cnoremap        <C-A> <Home>
cnoremap   <C-X><C-A> <C-A>
cnoremap        <C-B> <Left>
cnoremap        <C-F> <Right>
cnoremap   <C-X><C-F> <C-F>
cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "\<C-D>" : "\<Del>"

" ··········· positioning ····· {{{1
" push newline
nnoremap <S-CR>   mzO<Esc>`z
nnoremap <C-CR>   mzo<Esc>`z

" bubble up
nnoremap <silent> <C-Up>   mZ:silent! move .-2<CR>==`Z
vnoremap <silent> <C-Up>   :<C-U>silent! '<,'>move '<-2<CR>gv=gv
inoremap <silent> <C-Up>   <Esc>:silent! move .-2<CR>==gi
" bubble down
nnoremap <silent> <C-Down> mZ:silent! move .+1<CR>==`Z
vnoremap <silent> <C-Down> :<C-U>silent! '<,'>move '>+1<CR>gv=gv
inoremap <silent> <C-Down> <Esc>:silent! move .+1<CR>==gi

" emacs-like indentation
vnoremap <Tab> =
vnoremap     < <gv
vnoremap     > >gv

" ··········· editing ········· {{{1
" spawn newline
inoremap <S-CR> <C-O>O
inoremap <C-CR> <C-O>o

" sensible Y
nnoremap Y y$

" the abyss
vnoremap <BS> "_d

" start new undo-group for <C-U>
inoremap <C-U> <C-G>u<C-U>

" jump around insert mode
inoremap      <C-A> <Home>
inoremap <C-X><C-A> <C-A>
inoremap      <C-B> <C-G>U<Left>
inoremap      <C-F> <C-G>U<Right>
inoremap      JL    <End>

" edit like you're emacs
inoremap <C-D> <Del>
inoremap <C-T> <Esc>xpa

" + / -
nnoremap + <C-A>
nnoremap - <C-X>

" ··········· shell ··········· {{{1
nnoremap <leader>i <C-Z>

" ··········· autocompletion ·· {{{1
inoremap     <C-L> <C-X><C-L>
inoremap     <C-]> <C-X><C-]>
inoremap     <C-@> <C-X><C-O>
inoremap <C-Space> <C-X><C-O>

inoremap <expr> <Tab>   SuperTab("\<C-N>", "\<C-F>")
inoremap <expr> <S-Tab> SuperTab("\<C-P>", "\<C-D>")

" ··········· search ·········· {{{1
" emacs taking over my life
inoremap      <C-S>  <C-O>/
nnoremap          U  <C-R>
nnoremap <C-X><C-U>  U
nnoremap     <C-X>u  U

" NOTE: repeat.vim will avoid mapping U to RepeatRedo because I have a custom U mapping.
" If inconsistencies arise in repeat/undo/redo behavior, consider something like:
" nmap <expr> U  exists('g:loaded_repeat') ? "\<Plug>(RepeatRedo)" : "\<C-R>"

fun! IsSearch()
  let cmdtype = getcmdtype()
  return cmdtype == '/' || cmdtype == '?'
endf

cnoremap <expr> <C-P> IsSearch() ? "\<C-T>" : "\<Up>"
cnoremap <expr> <C-N> IsSearch() ? "\<C-G>" : "\<Down>"
cnoremap <expr> <C-Y> IsSearch() ? "\<C-L>" : "\<C-Y>"

" :nohlsearch
nnoremap <silent> <M-U> :nohlsearch<CR>
nnoremap coh :<C-R>=eval(&hls) ? (v:hlsearch ? 'noh' : 'set nohls') : 'set hls'<CR><CR>

" substitute
nnoremap <leader>S :%s/
nnoremap <leader>s :s/
vnoremap <leader>s :s/
nnoremap        c. *Ncgn

" preserve flags
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" ··········· movement ········ {{{1
" first non-blank on next/previous line
nnoremap <C-N> +
nnoremap <C-P> -

" scroll
noremap <C-J> <C-E>
noremap <C-K> <C-Y>
noremap <C-E> $
noremap <C-A> 0

" ··········· splits ·········· {{{1
nnoremap <C-D>      <C-W><C-Q>
noremap  <leader>,  <C-W>p
noremap  <C-W><C-Y> <C-W>z

" ··········· tabs ············ {{{1
noremap <C-W><C-N> gt
noremap <C-W><C-P> gT

" folding
nnoremap z. za

" emacs redraws
nnoremap <C-L>         zz
nnoremap <C-U><C-L>    zt
nnoremap <leader><C-L> <C-L>

" ··········· git ············· {{{1
noremap <silent> gb :Gblame<CR>
noremap <silent> gs :Gstatus<CR>
