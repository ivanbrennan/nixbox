" git
augroup GitGroup
  autocmd!
  au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
augroup END

" preview
augroup Preview
  autocmd!
  autocmd BufWinEnter * if &previewwindow | nnoremap <nowait><buffer> q <C-W>q| endif
augroup END

" VAM uses an autocmd to inoremap <C-X><C-P> to plugin name completion,
" which is less useful than the built-in functionality it clobbers.
" The docs say you can disable it via:
"   let g:vim_addon_manager.addon_completion_lhs = 0
" but this doesn't work, I'm guessing because Nix is evaluating things
" in such a way that VAM sets up the autocmd before the user can set
" different defaults.
" For now, work around this annoyance by nuking the fucking thing...
if exists('#VAM_addon_name_completion')
  augroup VAM_addon_name_completion
    autocmd!
  augroup END
  augroup! VAM_addon_name_completion
endif
" VAM has proved very flexible and useful, but this makes me wonder if
" it's actually an over-engineered mess.
