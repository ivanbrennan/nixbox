if string.match(vim.fn.expand('%:p'), '^fugitive:[\\/][\\/]') then
  vim.keymap.set('n', 'q', '<Cmd>bdelete<CR>', {
    buffer = true,
    nowait = true,
  })
end
