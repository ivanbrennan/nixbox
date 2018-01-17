" ::::::::: Options :::::::::::::::::::::::

" startup
set exrc           " enable local .vimrc files
set secure         " disable unsafe commands in local .vimrc files

" persistence
if !isdirectory($HOME."/.vim/tmp")
  call mkdir($HOME."/.vim/tmp", "p")
endif
if !isdirectory($HOME."/.vim/backup")
  call mkdir($HOME."/.vim/backup", "p")
endif
if !isdirectory($HOME."/.vim/undo")
  call mkdir($HOME."/.vim/undo", "p")
endif
set directory=~/.vim/tmp
set backup
set backupdir=~/.vim/backup
set undofile
set undodir=~/.vim/undo
set history=1000
set hidden

" tags
setg tags-=./tags    " don't just look in the current buffer's directory (vim)
setg tags-=./tags;   " don't just look in the current buffer's directory (nvim)
setg tags+=./tags;~  " search for tags recursively upwards until ~

" navigation
set incsearch
set ignorecase
set smartcase
set scrolloff=1
set sidescroll=2
set mouse+=a
set guioptions-=L
set guioptions-=r
set splitright
set splitbelow

" editing
set backspace=indent,eol,start
set nojoinspaces
set nrformats=
set complete-=i
set clipboard=unnamedplus " default to the clipboard register
if exists('+inccommand')
  set inccommand=nosplit  " nvim magic
endif

" formatting
set formatoptions-=t            " don't auto-wrap non-commented text
set formatoptions-=o            " don't auto-comment with o or O
set formatoptions+=r            " auto-comment with Enter
silent! set formatoptions+=j    " let J handle comments if supported

let g:colorcolumn_start=100
func! ColorColumnStart()
  return &textwidth > 0 ? &textwidth : g:colorcolumn_start
endf

augroup Formatting
  autocmd!
  autocmd FileType * execute 'setl formatprg=par\ -w'.ColorColumnStart()
augroup END

" appearance
set synmaxcol=256
set title
set nowrap
set showtabline=1
set fillchars=vert:│
set foldmethod=syntax
set foldlevelstart=10
set foldtext=MyFoldText()
set nofoldenable
set showcmd
set lazyredraw

" notifications
set shortmess+=I
set novisualbell

" matching
set showmatch
set matchtime=2

" whitespace
set tabstop=2                   " tab is two spaces
set softtabstop=2               " softtab is two spaces
set shiftwidth=2                " autoindent is two spaces
set expandtab                   " convert tabs to spaces

" characters
if exists('&guifont')
  set guifont=Source\ Code\ Pro:h14
endif
set list                         " show invisible characters
set listchars=""                 " reset the listchars
set listchars=tab:▸\             " tab
set listchars+=trail:·           " trailing space
set listchars+=extends:»         " continues offscreen
set listchars+=precedes:«        " precedes offscreen
