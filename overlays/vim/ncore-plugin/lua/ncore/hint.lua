local api = vim.api
local opt = vim.opt
local fn = vim.fn
local v = vim.v
local w = vim.w
local keymap = vim.keymap

local main_group_name = 'HintHighlight'
local cursor_group_name = 'HintCursor'
local highlight_name = 'IncSearch'

local main_group = api.nvim_create_augroup(main_group_name, {
  clear = true
})

-- Forward declarations
local set_hlsearch
local next_move_adds_hlmatch
local next_move_clears_highlight
local add_hlmatch
local delete_hlmatch
local hint_pattern
local is_highlighted
local clear_highlight

local prepare_highlight = function()
  set_hlsearch(true)
  next_move_adds_hlmatch()
end

hint_add_highlight = function()
  set_hlsearch(true)
  add_hlmatch()
end

local clear_highlight = function()
  pcall(api.nvim_del_augroup_by_name, cursor_group_name)
  set_hlsearch(false)
end

api.nvim_create_autocmd('CmdlineEnter', {
  group = main_group,
  pattern = { '[/\\?]' },
  callback = prepare_highlight,
})
api.nvim_create_autocmd('CmdlineEnter', {
  group = main_group,
  pattern = { '[>=@-]' },
  callback = clear_highlight,
})
api.nvim_create_autocmd({ 'InsertEnter', 'WinLeave' }, {
  group = main_group,
  callback = clear_highlight,
})
api.nvim_create_autocmd('WinEnter', {
  group = main_group,
  callback = function()
    -- If we switch into a window and there is no 'hlsearch' in effect but we
    -- do have a `w:hint_hlmatch` variable, it means that `:nohiglight` was
    -- probably run from another window and we should clear our match highlight
    -- and the window-local variable.
    if v.hlsearch ~= 1 then
      clear_highlight()
    end
  end,
})


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
    '<Cmd>lua hint_add_highlight()<CR>'
  }), { silent = true })
end

map('*',  '<Plug>(hint-*)')
map('#',  '<Plug>(hint-#)')
map('N',  '<Plug>(hint-N)')
map('g#', '<Plug>(hint-g#)')
map('g*', '<Plug>(hint-g*)')
map('n',  '<Plug>(hint-n)')
map('',   '<Plug>(hint_add_highlight)')

if not hasmapto('<Plug>(hint_toggle_highlight)') and not hasmapfrom('<M-u>', 'n') then
  keymap.set('n', '<M-u>', '<Plug>(hint_toggle_highlight)', {
    silent = true,
    remap = true,
    unique = true,
  })
end

if not hasmapto('<Plug>(hint_clear_highlight)') and not hasmapfrom('<Esc>', 'n') then
  keymap.set('n', '<Esc>', '<Plug>(hint_clear_highlight)<Cmd>diffupdate<Bar>normal! <C-l><CR>', {
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

keymap.set('n', '<Plug>(hint_toggle_highlight)', function()
  if is_highlighted() then
    clear_highlight()
  else
    hint_add_highlight()
  end
end, { silent = true, unique = true })

keymap.set('n', '<Plug>(hint_clear_highlight)', function()
  clear_highlight()
end, { silent = true, unique = true })

keymap.set('n', '<Plug>(hint_cword)', function()
  fn.setreg('/', '\\<' .. fn.expand('<cword>') .. '\\>')
  hint_add_highlight()
end, { silent = true, unique = true })

is_highlighted = function()
  return w.hint_hlmatch or v.hlsearch == 1
end

set_hlsearch = function(bool)
  delete_hlmatch()
  opt.hlsearch = bool
end

delete_hlmatch = function()
  if w.hint_hlmatch then
    pcall(fn.matchdelete, w.hint_hlmatch)
  end
  w.hint_hlmatch = nil
end

add_hlmatch = function()
  next_move_clears_highlight()
  local ok, id = pcall(fn.matchadd, highlight_name, hint_pattern())
  if ok then
    w.hint_hlmatch = id
  end
end

hint_pattern = function()
  -- Construct a pattern that matches the same text as the search string, but
  -- only when the cursor is positioned at the start of the match. Since the
  -- search may have involved lookbehind, we'll look for the \zs atom that would
  -- set the start of the match, and insert a \%# atom to match the cursor
  -- position following it. If no \zs if found, we'll place \%# at the beginning
  -- of the search string. In the obscure case where multiple \zs atoms were
  -- used, this approach fails gracefully -- no higlight will be added, but
  -- neither will an error occur.

  -- Get the full search string.
  local search = fn.getreg('/')

  -- Lookbehind prefix may contain, but not end with a backslash.
  return fn.substitute(search, [[\C\v^(%(.*[^\\])*%(\\\\)*\\zs)?]], [[\1\\%#\\c]], '')
end

next_move_clears_highlight = function()
  local group = api.nvim_create_augroup(cursor_group_name, {
    clear = true
  })
  api.nvim_create_autocmd('CursorMoved', {
    group = group,
    callback = clear_highlight,
  })
end

next_move_adds_hlmatch = function()
  local group = api.nvim_create_augroup(cursor_group_name, {
    clear = true
  })
  api.nvim_create_autocmd('CursorMoved', {
    group = group,
    callback = add_hlmatch,
  })
end
