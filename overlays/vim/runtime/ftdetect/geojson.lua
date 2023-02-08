vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = '*.geojson',
  command = 'setfiletype json',
})
