vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = '*.avsc',
  command = 'setfiletype json',
})
