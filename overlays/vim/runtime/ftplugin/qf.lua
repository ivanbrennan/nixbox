local set = vim.keymap.set

vim.opt_local.buflisted = false

-- quickfix list or location list
local c = #vim.fn.getloclist(0) == 0 and 'c' or 'l'

set('n', 'q', '<Cmd>'..c..'close<CR>', {
  buffer = true,
  silent = true,
  nowait = true,
})
