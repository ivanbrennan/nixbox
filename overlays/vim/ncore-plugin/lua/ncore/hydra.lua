local cmd = vim.cmd
local set = vim.keymap.set

local hydra = require('hydra')
local shared_config = {
  hint = {
    type = 'cmdline',
  },
  exit = false,
  foreign_keys = nil,
  on_enter = function()
    vim.o.lazyredraw = false
  end,
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
    { 'f',     '<C-f>', { desc = false } },
    { 'b',     '<C-b>', { desc = false } },
    { '<C-j>', '<C-e>', { desc = false } },
    { '<C-k>', '<C-y>', { desc = false } },
    { '<Esc>', nil, { exit = true, desc = false } },
  },
  config = shared_config,
})
set('n', '<C-f>', function()
  cmd.execute('"normal! \\<C-f>"')
  vertical_scroll:activate()
end)
set('n', '<C-b>', function()
  cmd.execute('"normal! \\<C-b>"')
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

local recenter_switch = 0
recenter = hydra({
  name = 'recenter',
  hint = ' ─ ',
  mode = 'n',
  heads = {
    {
      '<C-l>', function()
        if recenter_switch == 0 then
          cmd.execute('"normal! zz"')
        elseif recenter_switch == 1 then
          cmd.execute('"normal! zt"')
        else
          cmd.execute('"normal! zb"')
        end
        recenter_switch = (recenter_switch + 1) % 3
      end, { desc = false }
    },
    {
      '<Esc>', nil, { exit = true, desc = false }
    },
  },
  config = shared_config,
})
set('n', '<C-l>', function()
  cmd.execute('"normal! zz"')
  recenter_switch = 1
  recenter:activate()
end)
