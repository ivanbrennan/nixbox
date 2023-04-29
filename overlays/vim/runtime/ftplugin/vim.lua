vim.opt_local.foldmethod = 'marker'

-- evaluate contents of the current buffer
vim.keymap.set('n', '<Leader>vm', ':%y z <Bar> @z<CR>', {
  buffer = true,
  silent = true,
})
vim.keymap.set('x', '<Leader>vm', ':y z <Bar> @z<CR>',
{ buffer = true,
  silent = true,
})
