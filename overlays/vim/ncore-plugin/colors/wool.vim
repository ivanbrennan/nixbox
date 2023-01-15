" Vim color scheme
"
" Name:       wool.vim
" Maintainer: Ivan Brennan <ivan.brennan@gmail.com>
" License:    MIT

" https://jhacksworth.github.io/xterm-color-matcher/
" https://jhacksworth.github.io/xterm-color-matcher/#/c/983098
" https://framagit.org/gagbo/dotfiles/-/blob/master/scripts/bin/colortrans.py

" [["#0e1111"],["#1c1c1c"],["#1f1f1f"],["#1c1d21"],["#222327"],["#2c2f30"],["#28292d"],["#31343f"],["#3a3e4d"],["#737373"],["#757d80"],["#a9a9a9"],["#b8b8b8"],["#d3d3d3"],["#eeeeee"],["#dfe0e8"],["#b4a0c5"],["#778899"],["#c4d5e5"],["#ebc578"],["#f8bb39"],["#682421"],["#391716"],["#1d2717"],["#bbe068"],["#ecbe7b"],["#e80c4a"],["#5b96af"],["#6f82a6"]]

set background=dark

hi clear
if exists('syntax_on')
   syntax reset
endif

let g:colors_name = 'wool'

let s:black  = ['#0e1111', 233]
let s:grey00 = ['#1c1c1c', 234]
let s:grey01 = ['#1f1f1f', 234]
let s:grey02 = ['#1c1d21', 234]
let s:grey03 = ['#222327', 235]
let s:grey04 = ['#2c2f30', 236]
let s:grey05 = ['#28292d', 236]
let s:grey06 = ['#31343f', 237]
let s:grey07 = ['#3A3E4D', 237]
let s:grey08 = ['#737373', 243]
let s:grey09 = ['#757d80', 244]
let s:grey10 = ['#a9a9a9', 248]
let s:grey11 = ['#b8b8b8', 250]
let s:grey12 = ['#d3d3d3', 252]
let s:grey13 = ['#dfe0e8', 254]
let s:white  = ['#eeeeee', 255]

let s:magenta = ['#b4a0c5', 182]
let s:steel   = ['#5b96af', 67]
let s:shale   = ['#778899', 102]
let s:slate   = ['#6f82a6', 103]
let s:sky     = ['#c4d5e5', 153]
let s:yellow  = ['#ebc578', 222]
let s:teal    = ['#ecbe7b', 222]
let s:orange  = ['#f8bb39', 221]
let s:red     = ['#682421', 88]
let s:darkred = ['#391716', 52]
let s:green   = ['#1d2717', 65]
let s:lime    = ['#bbe068', 149]
let s:berry   = ['#e80c4a', 197]

let s:default_fg = s:grey12
let s:default_bg = s:grey03

let s:italic    = 'italic'
let s:bold      = 'bold'
let s:underline = 'underline'
let s:none      = 'NONE'

let s:none_lst    = [s:none, s:none]
let s:default_lst = []
let s:default_str = ''

if !exists("g:monochrome_italic_comments")
  let g:monochrome_italic_comments = 1
endif
let s:comment_attr = g:monochrome_italic_comments ? s:italic : s:none

function! s:hi(...) abort
    let group = a:1
    let fg    = get(a:, 2, s:default_fg)
    let bg    = get(a:, 3, s:default_bg)
    let attr  = get(a:, 4, s:default_str)
    let term  = get(a:, 4, s:default_str)

    let cmd = ['hi', group]

    if fg != s:default_lst
        call add(cmd, 'guifg='.fg[0])
        call add(cmd, 'ctermfg='.fg[1])
    endif

    if bg != s:default_lst
        call add(cmd, 'guibg='.bg[0])
        call add(cmd, 'ctermbg='.bg[1])
    endif

    if attr != s:default_str
        call add(cmd, 'gui='.attr)
        call add(cmd, 'cterm='.attr)
    endif

    if term != s:default_str
        call add(cmd, 'term='.term)
    endif

    exec join(cmd, ' ')
endfunction


"
" --- Vim interface ------------------------------------------------------------
"

call s:hi('Normal')
call s:hi('Cursor', s:black, s:grey12)
call s:hi('CursorLine', s:default_lst, s:grey05, s:none, s:none)
call s:hi('CursorLineNr', s:grey09, s:default_bg, s:none)
call s:hi('CursorColumn', s:default_lst, s:grey05, s:none, s:none)
call s:hi('ColorColumn', s:default_fg, s:grey00)
call s:hi('IncSearch', s:orange, s:grey05, s:bold)
call s:hi('Search', s:white, s:grey06, s:none)
call s:hi('Visual', s:default_lst, s:grey06)
call s:hi('Title', s:grey13, s:default_lst, s:bold)
call s:hi('ErrorMsg', s:lime, s:default_bg)
call s:hi('WarningMsg', s:orange, s:default_lst)
call s:hi('VertSplit', s:grey04, s:default_bg, s:none)
" call s:hi('WinSeparator', s:grey04, s:default_bg, s:none)
call s:hi('StatusLine', s:sky, s:grey02, s:none)
call s:hi('StatusLineNC', s:grey08, s:grey02, s:none)
call s:hi('Terminal', s:default_fg, s:grey00)
call s:hi('StatusLineTerm', s:grey12, s:grey02, s:none)
call s:hi('StatusLineTermNC', s:grey08, s:grey02, s:none)

" Tildes at the bottom of a buffer, etc.
call s:hi('NonText', s:grey10)
call s:hi('EndOfBuffer', s:grey00, s:grey00)

" Folding.
call s:hi('FoldColumn', s:grey10)
call s:hi('Folded')

" Line numbers gutter.
call s:hi('LineNr', s:grey07)

" Small arrow used for tabs.
call s:hi('SpecialKey', s:grey08, s:default_bg)

" File browsers.
call s:hi('Directory', s:grey13, s:default_bg, s:bold)

" Help.
call s:hi('helpSpecial')
call s:hi('helpHyperTextJump', s:shale, s:default_bg, s:underline)
call s:hi('helpNote')

" Popup menu.
call s:hi('Pmenu', s:grey09, s:grey00)
call s:hi('PmenuSel', s:sky, s:grey01, s:bold)
call s:hi('PmenuSbar', s:default_bg, s:default_bg)
call s:hi('PmenuThumb', s:grey10, s:grey06)

" Tabs.
call s:hi("TabLine", s:grey08, s:grey02, s:none)
call s:hi("TabLineFill", s:grey02, s:grey02, s:none)
call s:hi("TabLineSel", s:default_fg, s:default_bg, s:none)

" Spell.
call s:hi('SpellBad', s:berry, s:default_bg, s:none)
call s:hi('SpellCap', s:sky, s:default_bg, s:none)

" Notes.
call s:hi('Todo', s:lime, s:default_bg, s:bold)


"
" --- Programming languages ----------------------------------------------------
"

call s:hi('Statement', s:grey13, s:default_bg, s:bold)
call s:hi('Operator', s:yellow, s:default_bg, s:none)
call s:hi('PreProc', s:grey13, s:default_bg, s:bold)
call s:hi('Include', s:steel, s:default_bg, s:none)
call s:hi('String', s:teal)
call s:hi('Comment', s:slate, s:default_bg, s:comment_attr)
call s:hi('Number', s:magenta)
call s:hi('Constant', s:sky) " or yellow fg?
call s:hi('Type', s:default_fg, s:default_bg, s:bold)
call s:hi('Structure', s:steel, s:default_bg, s:none)
call s:hi('Function', s:grey13)
call s:hi('Identifier')
call s:hi('Delimiter')
call s:hi('Special', s:yellow)
call s:hi('MatchParen', s:lime, s:default_bg, s:bold)
call s:hi('Question', s:grey08, s:default_bg)
call s:hi('ModeMsg', s:grey09, s:default_bg, s:none)
call s:hi('MoreMsg', s:grey11, s:default_lst, s:bold)


"
" --- C ------------------------------------------------------------------------
"

hi link cCustomFunc Function
hi link cBoolean    Boolean

"
" --- VimL ---------------------------------------------------------------------
"

call s:hi('vimOption')
call s:hi('vimGroup')
call s:hi('vimHiClear')
call s:hi('vimHiGroup')
call s:hi('vimHiAttrib')
call s:hi('vimHiGui')
call s:hi('vimHiGuiFgBg')
call s:hi('vimHiCTerm')
call s:hi('vimHiCTermFgBg')
call s:hi('vimSynType')
hi link vimCommand Statement
hi link vimCommentTitle Comment
hi link vimLet vimCommand
hi link vimVar Identifier
hi link vimFuncVar Identifier
hi link vimEnvvar PreProc


"
" --- Ruby ---------------------------------------------------------------------
"

call s:hi('rubyConstant')
call s:hi('rubySharpBang', s:grey08)
call s:hi('rubyStringDelimiter', s:shale)
call s:hi('rubyStringEscape', s:shale)
call s:hi('rubyRegexpEscape', s:shale)
call s:hi('rubyRegexpAnchor', s:shale)
call s:hi('rubyRegexpSpecial', s:shale)


"
" --- Elixir -------------------------------------------------------------------
"

call s:hi('elixirAlias', s:default_fg, s:default_bg, s:none)
call s:hi('elixirDelimiter', s:shale)
call s:hi('elixirSelf', s:default_fg, s:default_bg, s:none)

" For ||, ->, etc.
call s:hi('elixirOperator')

" Module attributes like @doc or @type.
hi link elixirVariable Statement

" While rendered as comments in other languages, docstrings are strings,
" experimental.
hi link elixirDocString String
hi link elixirDocTest String
hi link elixirStringDelimiter String


"
" --- Perl ---------------------------------------------------------------------
"

call s:hi('perlSharpBang', s:grey08)
call s:hi('perlStringStartEnd', s:shale)
call s:hi('perlStringEscape', s:shale)
call s:hi('perlMatchStartEnd', s:shale)


"
" --- Python -------------------------------------------------------------------
"

call s:hi('pythonEscape', s:shale)


"
" --- JavaScript ---------------------------------------------------------------
"

call s:hi('javaScriptFunction', s:grey13, s:default_bg, s:bold)


"
" --- Diffs --------------------------------------------------------------------
"

call s:hi('diffFile', s:grey08)
call s:hi('diffNewFile', s:grey08)
call s:hi('diffIndexLine', s:grey08)
call s:hi('diffLine', s:grey08)
call s:hi('diffSubname', s:grey08)
call s:hi('DiffAdd', s:default_lst, s:green)
call s:hi('DiffDelete', s:grey00, s:grey00)
call s:hi('DiffChange', s:default_lst, s:darkred)
call s:hi('DiffText', s:grey12, s:red, s:none)
hi link diffAdded DiffAdd
hi link diffRemoved DiffChange

"
" --- Markdown -----------------------------------------------------------------
"

call s:hi('markdownHeadingDelimiter', s:grey13, s:default_bg, s:bold)
call s:hi('markdownHeadingRule', s:grey13, s:default_bg, s:bold)
call s:hi('markdownLinkText', s:shale, s:default_bg, s:underline)


"
" --- vim-fugitive -------------------------------------------------------------
"

call s:hi('gitcommitComment', s:default_fg, s:default_bg, s:none)
call s:hi('gitcommitOnBranch', s:default_fg, s:default_bg, s:none)
call s:hi('gitcommitBranch', s:shale, s:default_bg, s:none)
call s:hi('gitcommitHeader', s:grey13, s:default_bg, s:bold)
call s:hi('gitcommitSelected', s:default_fg, s:default_bg, s:none)
call s:hi('gitcommitDiscarded', s:default_fg, s:default_bg, s:none)
call s:hi('gitcommitSelectedType', s:default_fg, s:default_bg, s:none)
call s:hi('gitcommitDiscardedType', s:default_fg, s:default_bg, s:none)


"
" --- NeoMake ------------------------------------------------------------------
"

call s:hi('NeomakeMessageSign')
call s:hi('NeomakeWarningSign', s:shale)
call s:hi('NeomakeErrorSign', s:yellow)
call s:hi('NeomakeInfoSign')
call s:hi('NeomakeError', s:yellow)
call s:hi('NeomakeInfo', s:default_fg, s:default_bg, s:bold)
call s:hi('NeomakeMessage')
call s:hi('NeomakeWarning', s:yellow)


"
" --- Traces ------------------------------------------------------------------
"

call s:hi('TracesSearch', s:orange, s:grey05, s:none)



"
" --- clever-f ----------------------------------------------------------------
"

call s:hi('CleverFChar', s:orange, s:grey05, s:bold)
