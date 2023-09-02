" This file is sourced early in the initialization process.
"
" I've extracted most customizations to into plugins organized using Vim 8's
" package feature. The only settings that need to remain in vimrc are those
" that must be set before loading other runtime files, plugins, or packages.

if exists('g:loaded_vimrc') | finish | endif
let g:loaded_vimrc = 1

let g:loaded_netrwPlugin = 1 " disable netrw (use dirvish instead)
let g:is_bash=1              " sh is bash
let g:sh_fold_enabled=1      " fold sh functions
let g:vimsyn_folding = "f"   " fold vim functions
let g:vimsyn_noerror = 1     " vim.vim sometimes gets it wrong
let g:fugitive_no_maps = 1   " leave me free to remap <C-R>

silent! source ~/.vim/vimrc.local
