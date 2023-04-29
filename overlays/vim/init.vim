" This file is sourced early in the initialization process.
"
" I've extracted most configuration to into plugins organized using packages.
" The only options that need to remain here are those that need to be set
" before loading other runtime files, plugins, or packages.

if exists('g:loaded_vimrc') | finish | endif
let g:loaded_vimrc = 1

let g:is_bash = 1            " sh is bash
let g:sh_fold_enabled = 1    " fold sh functions
let g:vimsyn_folding = 'f'   " fold vim functions
let g:vimsyn_noerror = 1     " vim.vim sometimes gets it wrong
let g:fugitive_no_maps = 1   " leave me free to remap <C-R>
let g:loaded_netrwPlugin = 1 " disable netrw (use dirvish instead)

augroup RestoreCursor
  autocmd!
  autocmd BufRead * autocmd FileType <buffer> ++once call s:restore_cursor_position()
augroup end

func! s:restore_cursor_position() abort
  if &ft !~# 'commit\|rebase' && line("'\"") > 1 && line("'\"") <= line("$")
    exec 'normal! g`"'
  endif
endf

" ~/.config/nvim/local.vim
execute 'silent! source ' . stdpath('config') . '/local.vim'

" ~/.config/nvim/lua/user/local.lua
lua pcall(require, 'user/local')
