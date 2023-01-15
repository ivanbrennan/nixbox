--[[ Defaults
vim.keymap.set('n', 'Y', 'y$')
vim.keymap.set('i', '<C-U>', '<C-G>u<C-U>')
vim.keymap.set('i', '<C-W>', '<C-G>u<C-W>')
vim.keymap.set('x', '*', 'y/\V<C-R>"<CR>')
vim.keymap.set('x', '#', 'y?\V<C-R>"<CR>')
vim.keymap.set('n', '&', ':&&<CR>')
--]]

-- We can now map Tab and C-i separately, but what should we map Tab to?
vim.keymap.set('n', '<C-i>', '<C-i>')
vim.keymap.set('n', '<Tab>', '<Cmd>bnext<CR>')
vim.keymap.set('n', '<S-Tab>', '<Cmd>bprevious<CR>')

vim.g.mapleader = ' '
vim.keymap.set('n', '<M-u>', '<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>')
vim.keymap.set('n', '<leader>.', ":edit <C-R>=empty(expand('%')) ? '' : expand('%:~:.:h').'/'<CR>")
vim.keymap.set('n', '<leader>h', ':help ')
vim.keymap.set('n', '<leader>fs', ':write<CR>')
vim.keymap.set('n', '<C-D>', '<C-W><C-Q>')
vim.keymap.set('n', 'U', '<C-R>')
