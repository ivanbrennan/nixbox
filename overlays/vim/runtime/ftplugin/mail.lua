local ol = vim.opt_local
local set = vim.keymap.set

ol.list = false
ol.spell = true
ol.synmaxcol = 0
ol.wrap = true

set('n', 'j', 'gj', { buffer = true })
set('n', 'k', 'gk', { buffer = true })
