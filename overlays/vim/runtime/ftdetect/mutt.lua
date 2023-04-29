vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = '*.mutt',
  command = 'setfiletype muttrc',
})
