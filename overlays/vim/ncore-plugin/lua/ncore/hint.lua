local api = vim.api
local create_augroup = api.nvim_create_augroup
local create_autocmd = api.nvim_create_autocmd

local opt = vim.opt
local fn = vim.fn
local v = vim.v
local w = vim.w
local keymap = vim.keymap

local main_group = create_augroup('Hint', { clear = true })

local cursor_group_name = 'HintCursor'
local highlight_name = 'IncSearch'

local delete_hlmatch = function()
  if w.hint_hlmatch then
    pcall(fn.matchdelete, w.hint_hlmatch)
  end
  w.hint_hlmatch = nil
end

local set_hlsearch = function(bool)
  delete_hlmatch()
  opt.hlsearch = bool
end

local clear_highlight = function()
  pcall(api.nvim_del_augroup_by_name, cursor_group_name)
  set_hlsearch(false)
end

local next_move_clears_highlight = function()
  local cursor_group = create_augroup(cursor_group_name, {
    clear = true
  })
  create_autocmd('CursorMoved', {
    group = cursor_group,
    callback = clear_highlight,
  })
end

local pattern_at_cursor = function()
  -- Construct a pattern that matches the same text as the search string, but
  -- only when the cursor is positioned at the start of the match. This is done
  -- by inserting \%# (a cursor-position pattern atom) into the search string.
  --
  -- If the search string contains \zs (marking the beginning of the match),
  -- insert \%# immediately following it. Otherwise, position \%# at the
  -- beginning of the search string.
  --
  -- This pattern will also work on searches that contain lookahead/lookbehind
  -- atoms (\@<= \@<! \@= \@!), or any number of \zs atoms, but it won't work on
  -- a search that embedded \zs in a repeated group (e.g. \(A\zsB\)\{2}).

  -- Get the full search string.
  local search = fn.getreg('/')

  -- The prefix leading up to \zs may contain backslashes, but may not end with
  -- an unescaped backslash.
  return fn.substitute(search, [[\C\v^(%(.*[^\\])*%(\\\\)*\\zs)?]], [[\1\\%#\\c]], '')
end

local add_hlmatch = function()
  next_move_clears_highlight()
  local ok, id = pcall(fn.matchadd, highlight_name, pattern_at_cursor())
  if ok then
    w.hint_hlmatch = id
  end
end

local next_move_adds_hlmatch = function()
  local cursor_group = create_augroup(cursor_group_name, {
    clear = true
  })
  create_autocmd('CursorMoved', {
    group = cursor_group,
    callback = add_hlmatch,
  })
end

local is_highlighted = function()
  return w.hint_hlmatch or v.hlsearch == 1
end

local add_highlight = function()
  set_hlsearch(true)
  add_hlmatch()
end

create_autocmd('CmdlineEnter', {
  group = main_group,
  pattern = { '[/\\?]' },
  callback = function()
    set_hlsearch(true)
    next_move_adds_hlmatch()
  end,
})

create_autocmd('CmdlineEnter', {
  group = main_group,
  pattern = { '[>=@-]' },
  callback = clear_highlight,
})

create_autocmd({ 'InsertEnter', 'WinLeave' }, {
  group = main_group,
  callback = clear_highlight,
})

create_autocmd('WinEnter', {
  group = main_group,
  callback = function()
    -- If we switch into a window and there is no hlsearch in effect but we do
    -- have a w.hint_hlmatch variable, it means that :nohiglight was probably
    -- run from another window and we should clear our match highlight and the
    -- window-local variable.
    if v.hlsearch ~= 1 then
      clear_highlight()
    end
  end,
})

api.nvim_create_user_command('HintHighlight', function(_)
  add_highlight()
end, {})

local hasmapfrom = function(keys, mode)
  return #fn.maparg(keys, mode) > 0
end

local hasmapto = function(target)
  return fn.hasmapto(plug) == 1
end

local map = function(keys, plug)
  if #keys > 0 and not hasmapto(plug) then
    keymap.set('n', keys, plug, { silent = true, remap = true })
  end
  keymap.set('n', plug, table.concat({
    keys,
    'zv',
    '<Cmd>HintHighlight<CR>'
  }), { silent = true })
end

map('*',  '<Plug>(hint-*)')
map('#',  '<Plug>(hint-#)')
map('N',  '<Plug>(hint-N)')
map('g#', '<Plug>(hint-g#)')
map('g*', '<Plug>(hint-g*)')
map('n',  '<Plug>(hint-n)')
map('',   '<Plug>(hint_highlight)')

if not hasmapto('<Plug>(hint_toggle)') and not hasmapfrom('<M-u>', 'n') then
  keymap.set('n', '<M-u>', '<Plug>(hint_toggle)', {
    silent = true,
    remap = true,
    unique = true,
  })
end

if not hasmapto('<Plug>(hint_clear)') and not hasmapfrom('<Esc>', 'n') then
  keymap.set('n', '<Esc>', '<Plug>(hint_clear)<Cmd>diffupdate<CR>', {
    silent = true,
    remap = true,
    unique = true,
  })
end

if not hasmapfrom('<Leader><C-l>', 'n') then
  keymap.set('n', '<Leader><C-l>', table.concat({
    '<Plug>(hint_clear)',
    '<Cmd>diffupdate',
    '<Bar>normal! <C-l><CR>'
  }), {
    silent = true,
    remap = true,
    unique = true,
  })
end

if not hasmapto('<Plug>(hint_cword)') and not hasmapfrom('<M-U>', 'n') then
  keymap.set('n', '<M-U>', '<Plug>(hint_cword)', {
    silent = true,
    remap = true,
    unique = true,
  })
end

keymap.set('n', '<Plug>(hint_toggle)', function()
  if is_highlighted() then
    clear_highlight()
  else
    add_highlight()
  end
end, { silent = true, unique = true })

keymap.set('n', '<Plug>(hint_clear)', function()
  clear_highlight()
end, { silent = true, unique = true })

keymap.set('n', '<Plug>(hint_cword)', function()
  fn.setreg('/', '\\<' .. fn.expand('<cword>') .. '\\>')
  add_highlight()
end, { silent = true, unique = true })
