vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = '*.bats',
  command = 'setfiletype sh',
})
