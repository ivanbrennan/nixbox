g = vim.g
ol = vim.opt_local
set = vim.keymap.set

ol.omnifunc = 'rubycomplete#Complete'
ol.foldmethod = 'indent'
ol.foldlevel = 99

-- complete buffer loading can cause code execution
-- turn this off if it's a concern
g.rubycomplete_buffer_loading = 1
g.rubycomplete_classes_in_global = 1
g.rubycomplete_rails = 1

g.ruby_operators = 1
vim.cmd('syn match parens /[(){}\\[\\]]/')
vim.api.nvim_set_hl(0, 'parens', { link = 'Delimiter', default = true })

-- vim-ruby provides a command-line mapping for <Plug><ctag> that
-- intelligently identifies the current Ruby cursor identifier.
set('n', '<Plug>(ArticulateTag)', ':<C-u>exe v:count1 "tag <Plug><ctag>"<CR>', {
  buffer = true,
  silent = true,
  remap  = true,
})
set('n', '<Plug>(ArticulateTjump)', ':<C-u>tjump <Plug><ctag><CR>', {
  buffer = true,
  silent = true,
  remap  = true,
})
