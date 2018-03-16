let g:edot_vimrc = glob(resolve($HOME.'/.vim/vimrc-before.vim'))

let g:optcycle_config = {
\   'colorscheme': [
\     { 'colorscheme': 'root' },
\     { 'colorscheme': 'moot' },
\     { 'colorscheme': 'ion' },
\     { 'colorscheme': 'blight' }
\   ]
\ }

let g:projectionist_heuristics = {
\   '*': {
\     '*.c': {
\       'alternate': '{}.h',
\       'type': 'source'
\     },
\     '*.h': {
\       'alternate': '{}.c',
\       'type': 'header'
\     }
\   }
\ }

let g:dirvish_mode = ':sort r /[^\/]$/'

let g:LoupeCenterResults = 0
let g:LoupeHlSearchTimeout = 1000
let g:LoupeVeryMagic = 0
