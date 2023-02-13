local hydra = require('hydra')
local shared_config = {
  hint = {
    type = 'cmdline',
  },
  exit = false,
  foreign_keys = nil,
}

hydra({
  name = 'horizontal-scroll',
  hint = ' ↔ ',
  mode = 'n',
  body = 'z',
  heads = {
    { 'L', 'zL', { desc = false } },
    { 'l', 'zl', { desc = false } },
    { 'h', 'zh', { desc = false } },
    { 'H', 'zH', { desc = false } },
    { '<Esc>', nil, { exit = true, desc = false } },
  },
  config = shared_config,
})

local vertical_scroll = hydra({
  name = 'vertical-scroll',
  hint = ' ↕ ',
  mode = 'n',
  heads = {
    { 'f', '<C-f>', { desc = false } },
    { 'b', '<C-b>', { desc = false } },
    { 'j', '<C-e>', { desc = false } },
    { 'k', '<C-y>', { desc = false } },
    { '<Esc>', nil, { exit = true, desc = false } },
  },
  config = shared_config,
})
vim.keymap.set('n', '<C-f>', function()
  vim.cmd.execute('"normal! \\<C-f>"')
  vertical_scroll:activate()
end)
vim.keymap.set('n', '<C-b>', function()
  vim.cmd.execute('"normal! \\<C-b>"')
  vertical_scroll:activate()
end)

hydra({
  name = 'window',
  hint = ' ▢ ',
  mode = 'n',
  body = '<C-w>',
  heads = {
    { '<', '<C-w><',     { desc = false } },
    { '>', '<C-w>>',     { desc = false } },
    { '-', '<C-w>-',     { desc = false } },
    { '+', '<C-w>+',     { desc = false } },
    { '\\', '<C-w>\\',   { desc = false } },
    { '_', '<C-w>_',     { desc = false } },
    { '=', '<C-w>=',     { desc = false } },
    { ';', '<C-w><C-w>', { desc = false } },
    { 'r', '<C-w>r',     { desc = false } },
    { 'R', '<C-w>R',     { desc = false } },
    { 'x', '<C-w>x',     { desc = false } },
    { '<Esc>', nil,      { exit = true, desc = false } },
  },
  config = shared_config,
})
