local cmd = vim.cmd
local fn = vim.fn
local set = vim.keymap.set

if vim.w.quickfix_title then
  vim.opt_local.statusline = table.concat({
    ' ',
    '%4*',                 -- italics
    '%{w:quickfix_title}',
    '%*',                  -- reset highlight group
    ' ',
    '%=',                  -- separator
    '%l',                  -- line
    '%#StatusLineNC#',     -- dim
    '/',
    '%*',                  -- reset highlight group
    '%L',                  -- total lines
    ' ',
  })
end

set('n', '<Plug>(latitude)', function()
  -- buffer-relative
  local curs_line = fn.line('.')
  local last_line = fn.line('$')
  -- window-relative
  local winheight = fn.winheight(0)
  local winline = fn.winline()

  local margin = math.min(6, math.floor(winheight/2))

  local bot_want = math.min(margin, last_line - curs_line)
  local bot_have = winheight - winline
  if bot_want > bot_have then
    cmd('normal! ' .. bot_want - bot_have .. '<C-e>')
  else
    local top_want = math.min(margin, curs_line - 1)
    local top_have = winline - 1
    if top_want > top_have then
      cmd('normal! ' .. top_want - top_have .. '<C-y>')
    end
  end
end)

set('n', '<CR>', '<Plug>(ArticulateEnter)<Plug>(latitude)', {
  buffer = true,
  silent = true,
  remap  = true,
})
