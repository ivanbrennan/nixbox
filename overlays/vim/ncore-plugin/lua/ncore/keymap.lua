local set = vim.keymap.set
local api = vim.api
local cmd = vim.cmd
local fn = vim.fn
local opt = vim.opt
local o = vim.o
local bo = vim.bo
local wo = vim.wo

--[[ Defaults
set('n', 'Y', 'y$')
set('i', '<C-u>', '<C-g>u<C-u>')
set('i', '<C-w>', '<C-g>u<C-w>')
set('x', '*', 'y/\V<C-r>"<CR>')
set('x', '#', 'y?\V<C-r>"<CR>')
set('n', '&', ':&&<CR>')
--]]

vim.g.mapleader = ' '
-- Don't move the cursor if a leader mapping times out.
set('n', '<Space>', '<Nop>', { silent = true})

-- buffers / files
set('n', '<Leader>o', function()
  if #fn.expand('%') > 0 then
    return ':edit ' .. fn.expand('%:~:.:h') .. '/'
  else
    return ':edit '
  end
end, { expr = true })
set('n', '<Leader>e', ':edit ')
set('n', '<Leader>.', ':edit **/')
set('n', '<Leader>fs', '<Cmd>write<CR>')
set('n', '<Leader>wq', '<Cmd>wq<CR>')
set('n', '<Leader>dd', '<Cmd>bdelete<CR>')
-- TODO: I use this <Leader>dl binding when I want to delete a buffer
-- without closing the window it's occupying. I switch to a different
-- buffer, then use this to delete the (now alternate) buffer. Let's
-- find a more straightforward (1-step) solution.
set('n', '<Leader>dl', '<Cmd>bdelete#<CR>')
set('n', '<Leader>l', '<C-^>')
set('n', '<Leader>F', ':setf ')
set('n', '<C-i>', '<C-i>') -- Distinguish <C-i> from <Tab>
set('n', '<Tab>', '<Cmd>bnext<CR>')
set('n', '<S-Tab>', '<Cmd>bprevious<CR>')

-- " quickfix/loclist
-- nmap     <Leader><Space> <Plug>(listical_toggle)
-- nmap     <M-n>           <Plug>(listical_next)<Plug>
-- nmap     <M-N>           <Plug>(listical_next_file)<Plug>
-- nmap     <M-p>           <Plug>(listical_previous)<Plug>
-- nmap     <M-P>           <Plug>(listical_previous_file)<Plug>
-- nmap     <M-h>           <Plug>(listical_older)
-- nmap     <M-l>           <Plug>(listical_newer)

-- cmdline
set({ 'n', 'v' }, ';', ':')
set({ 'n', 'v' }, "'", ';')
set({ 'n', 'v' }, 'q;', 'q:')
set('n', '<Leader>x', ':!')
set('n', '<Leader>h', ':help ')
set('n', '<Leader>H', ':help <C-r><C-w>')
--set('c', ';', '<Plug>(refract_semicolon_recall)', { remap = true })
--set('c', 's', '<Plug>(refract_autoreturn_ls_vs)', { remap = true })

-- add blank line above / below
set('n', '<S-CR>', '<Cmd>call append(line(".") - 1, "")<CR>')
set('n', '<C-CR>', '<Cmd>call append(line("."), "")<CR>')

-- bubble up / down
set('n', '<C-Up>', 'mZ<Cmd>silent! move .-2<CR>==`Z')
set('v', '<C-Up>', "<Esc><Cmd>silent! '<,'>move '<-2<CR>gv=gv")
set('i', '<C-Up>', '<Esc><Cmd>silent! move .-2<CR>==gi')
set('n', '<C-Down>', 'mZ<Cmd>silent! move .+1<CR>==`Z')
set('v', '<C-Down>', "<Esc><Cmd>silent! '<,'>move '>+1<CR>gv=gv")
set('i', '<C-Down>', '<Esc><Cmd>silent! move .+1<CR>==gi')

-- spawn newline
set('i', '<S-CR>', '<C-o>O')
set('i', '<C-CR>', '<C-o>o')
set('i', '<M-CR>', '<C-o>o')

-- the abyss
set('v', '<BS>', '"_d')

local CharKeys = { ['\t'] = '<C-v><Tab>' }
setmetatable(CharKeys, {
  __index = function(t, c)
    t[c] = c
    return c
  end
})

-- TODO: move these functions into a separate module?
local match_char = function(line, col)
  return CharKeys[fn.matchstr(line, [[\%]] .. col .. 'c.')]
end

local match_previous_char = function(line, col)
  return CharKeys[fn.matchstr(line, [[.\%]] .. col .. 'c')]
end

local match_pre_previous_char = function(line, col)
  return CharKeys[fn.matchstr(line, [[.\ze.\%]] .. col .. 'c')]
end

local transpose_preceding = function(line, col)
  local pre_prev_char = match_pre_previous_char(line, col)
  local prev_char = match_previous_char(line, col)

  if pre_prev_char == '' then
    return '<C-g>U<Left>'
  elseif pre_prev_char == prev_char then
    return ''
  else
    return '<BS><BS>' .. prev_char .. pre_prev_char
  end
end

local transpose_surrounding = function(line, col)
  return "<BS><Del>" .. match_char(line, col) .. match_previous_char(line, col)
end

local transpose = function(line, col)
  if col == 1 then
    return ''
  elseif col > #line then
    return transpose_preceding(line, col)
  else
    return transpose_surrounding(line, col)
  end
end

-- touch of Emacs
set({ 'n', 'v', 's' }, '<C-e>', '$')
set({ 'n', 'v', 's' }, '<C-a>', '0')
set('i', '<C-b>', '<Left>')
set('c', '<C-b>', '<Left>')
set('i', '<C-f>', '<Right>')
set('c', '<C-f>', '<Right>')
set('i', '<M-b>', '<C-Left>')
set('c', '<M-b>', '<C-Left>')
set('i', '<M-f>', '<C-Right>')
set('c', '<M-f>', '<C-Right>')
set('i', '<C-a>', function()
  local pattern = [[^\s*\%]] .. fn.col('.') .. [[c\S]]
  local i = fn.match(api.nvim_get_current_line(), pattern)
  return i >= 0 and '<Home>' or '<Esc>I'
end, { expr = true })
set('i', '<M-y>', '<C-a>')
set('c', '<C-a>', '<Home>')
set('c', '<M-a>', '<C-a>')
set('i', '<C-t>', function()
  return transpose(fn.getline('.'), fn.col('.'))
end, { expr = true })
set('c', '<C-t>', function()
  return transpose(fn.getcmdline(), fn.getcmdpos())
end, { expr = true })
set('i', '<C-d>', '<Del>')
set('c', '<C-d>', function()
  return fn.getcmdpos() > #fn.getcmdline() and '<C-d>' or '<Del>'
end, { expr = true })
set('i', '<M-d>', '<C-o>de')
set('c', '<M-d>', function()
  local line = fn.getcmdline()
  local pos = fn.getcmdpos()
  local ms = fn.matchlist(line, [[\(.*\%]] .. pos .. [[c\)\s*\w*\(.*\)]])
  fn.setcmdline(ms[2] .. ms[3], pos)
end)
set('i', '<C-s>', '<C-o>/')
set('i', '<C-r>', '<C-o>?')
set('i', '<M-r>', '<C-r>')
set('n', 'U', '<C-r>')
set('n', '<C-x><C-u>', 'U')
set('n', '<C-x>u', 'U')
local is_search = function()
  local t = fn.getcmdtype()
  return t == '/' or t == '?'
end
set('c', '<C-p>', function()
  return o.incsearch and is_search() and '<C-t>' or '<C-p>'
end, { expr = true })
set('c', '<C-n>', function()
  return o.incsearch and is_search() and '<C-g>' or '<C-n>'
end, { expr = true })
set('c', '<M-p>', '<Up>')
set('c', '<M-n>', '<Down>')
-- TODO:
-- set('c', '<C-r>', reverse-search-history)
set('c', '<M-r>', '<C-r>')
set('n', '<C-n>', '+')
set('n', '<C-p>', '-')

opt.cedit = '<C-o>'

-- touch of shell
local special_buftype = [[\C\v^%(help|quickfix|nowrite|nofile)$]]

local is_regular_buffer = function(bufnr)
  local o = bo[bufnr]
  return fn.match(o.buftype, special_buftype) == -1 and o.buflisted
end

local win_get_buf = api.nvim_win_get_buf

local any_other_regular_windows = function()
  local windows = api.nvim_list_wins()
  local current_win = api.nvim_get_current_win()

  for i=1, #windows do
    local w = windows[i]
    if w ~= current_win and is_regular_buffer(win_get_buf(w)) and not wo[w].previewwindow then
      return true
    end
  end
  return false
end

local any_modified_buffers = function()
  local buffers = api.nvim_list_bufs()

  for i=1, #buffers do
    if bo[buffers[i]].modified then
      return true
    end
  end
  return false
end

local any_other_regular_file_buffers = function()
  local buffers = api.nvim_list_bufs()
  local current_buf = api.nvim_get_current_buf()

  for i=1, #buffers do
    local b = buffers[i]
    if b ~= current_buf and is_regular_buffer(b) and fn.filereadable(api.nvim_buf_get_name(b)) == 1 then
      return true
    end
  end
  return false
end

local confirm_exit = function(prompt)
  return fn.confirm(prompt, 'y\nn') == 1
end

set('n', '<C-d>', function()
  if fn.tabpagenr('$') > 1 or any_other_regular_windows() then
    -- With at least one other tab and/or regular window present, we can close
    -- the current window without exiting nvim.
    cmd.quit()
  elseif any_modified_buffers() then
    -- Closing the current window will exit nvim. Since there are unsaved
    -- changes, prompt for confirmation first.
    if confirm_exit('Modified buffers exist. Exit anyway?') then
      cmd.quitall({ bang = true })
    end
  elseif any_other_regular_file_buffers() then
    -- Closing the current window will exit nvim. Since there are other
    -- file-backed buffers other than the current one, prompt to confirm first.
    if confirm_exit('Exit?') then
      cmd.quit()
    end
  else
    -- Closing the current window will exit nvim, but there are no unsaved
    -- changes and no file-backed buffers to worry about.
    cmd.quit()
  end
end)

set('n', '<Leader>qq', function()
  if any_modified_buffers() then
    if confirm_exit('Modified buffers exist. Exit anyway?') then
      cmd.quitall({ bang = true })
    end
  else
    cmd.quitall()
  end
end)

set('n', '<C-M-d>', '<Cmd>buffer #<Bar>bdelete #<CR>')

-- + -
set('n', '+', '<C-a>')
set('x', '+', '<C-a>')
set('n', '_', '<C-x>')
set('x', '_', '<C-x>')

-- scroll
set({ 'n', 'v', 's' }, '<C-j>', '<C-e>')
set({ 'n', 'v', 's' }, '<C-k>', '<C-y>')

-- commentary
set({ 'x', 'n', 'o' }, '<Leader>;', '<Plug>Commentary', { remap = true })
set('n', '<Leader>;;', '<Plug>CommentaryLine', { remap = true })

-- " EasyAlign
-- nmap     ga          <Plug>(EasyAlign)
-- xmap     ga          <Plug>(EasyAlign)
-- nnoremap gA          ga
-- xnoremap gA          ga

-- Articulated keybindings. These <Plug> pseudokeys provide a common interface
-- I can target when setting custom keymaps, e.g. in ftplugin files.

set({ 'n', 'x' }, '<Plug>(ArticulateTag)', '<C-]>')    -- tag
set({ 'n', 'x' }, '<Plug>(ArticulatePop)', '<C-T>')    -- pop
set({ 'n', 'x' }, '<Plug>(ArticulateTjump)', 'g<C-]>') -- tjump

-- tag, pop
set('n', '<C-.>', '<Plug>(ArticulateTag)', { remap = true })
set('x', '<C-.>', '<Plug>(ArticulateTag)', { remap = true })
set('n', '<C-,>', '<Plug>(ArticulatePop)', { remap = true })
set('x', '<C-,>', '<Plug>(ArticulatePop)', { remap = true })
set('n', 'g.', '<Plug>(ArticulateTjump)', { remap = true })
set('x', 'g.', '<Plug>(ArticulateTjump)', { remap = true })
set('n', 'g:', ':tjump ')

-- telescope
local telescope = require('telescope.builtin')
set('n', '<M-o>', telescope.find_files)
set('n', '<Leader>fg', telescope.live_grep)
set('n', '<Leader>/', telescope.live_grep)
set('n', '<Leader>fh', telescope.help_tags)
set('n', '<Leader>,', telescope.buffers)
set('n', 'g<Space>', ':tjump ')
-- nnoremap <silent> g<leader>     :Grepper<CR>
set('n', '<C-;>', '<Cmd>ls<CR>')
-- nnoremap <silent> <C-;>         :Buffers<CR>
-- nnoremap <silent> <leader>fh    :History/<CR>
-- nnoremap <silent> <leader>fg    :Tags<CR>
-- nnoremap <silent> <M-H>         :Helptags<CR>
-- cnoremap <expr>     :   refract#if_cmd_match(['^$'], "Commands\<CR>", ':')
-- cnoremap <expr>   <C-R> refract#if_cmd_match(['^$'], "History:\<CR>", "\<C-R>")

-- terminal keys
set('t', '<C-w>h',     [[<C-\><C-n><C-w>h]])
set('t', '<C-w>j',     [[<C-\><C-n><C-w>j]])
set('t', '<C-w>k',     [[<C-\><C-n><C-w>k]])
set('t', '<C-w>l',     [[<C-\><C-n><C-w>l]])
set('t', '<C-w><C-h>', [[<C-\><C-n><C-w>h]])
set('t', '<C-w><C-j>', [[<C-\><C-n><C-w>j]])
set('t', '<C-w><C-k>', [[<C-\><C-n><C-w>k]])
set('t', '<C-w><C-l>', [[<C-\><C-n><C-w>l]])
set('t', '<C-w>;',     [[<C-\><C-n>:]])
set('t', '<C-w><C-;>', [[<C-\><C-n>:]])

-- shell
set('n', '<Leader>i', '<C-z>', { remap = true })

-- autocompletion
set('i', '<C-;>', '<C-X><C-f>')
set('i', '<C-l>', '<C-X><C-l>')
set('i', '<C-.>', '<C-X><C-.>')

-- indentation
set('v', '<Tab>', '=')
set('v', '<', '<gv')
set('v', '>', '>gv')

-- substitute
set('n', '<M-s>', ':%s/')
set('n', '<Leader>s', ':s/')
set('v', '<Leader>s', ':s/')

-- replace word under cursor
set('n', 'c.', function()
  fn.setreg('/', [[\<]] .. fn.expand('<cword>') .. [[\>]])
  return 'cgn'
end, { expr = true })

-- spell
set('', 'z<CR>', '1z=')

--[[
  Safe <CR> for use in nmap's, cmap's, etc.
  Define a plug mapping to <CR>. This can be used for things like:

    nmap <CR> <Plug>(ArticulateEnter)<Plug>(hint_highlight)

  (without causing infinite recursion)
--]]
set({'n','v','o','s','i','c','t'}, '<Plug>(ArticulateEnter)', '<CR>')

-- zoom
set('n', '<Plug>(ArticulateZoom)', function()
  if fn.winnr('$') > 1 then
    cmd('tab sbuffer %')
  elseif fn.tabpagenr('$') > 1 then
    cmd('tabclose!')
  end
end)

set('n', '<CR>', function()
  if bo.filetype == 'qf' then
    return '<Plug>(ArticulateEnter)'
  else
    return '<Plug>(ArticulateZoom)'
  end
end, { expr = true })

set('n', '<Leader><Tab>', function()
  local i = o.showtabline
  if i == 2 or (i == 1 and fn.tabpagenr('$') > 1) then
    opt.showtabline = 0
  else
    opt.showtabline = 2
  end
end)

set('n', '<Leader><Esc>', function()
  local i = o.laststatus
  if i < 3 then
    opt.laststatus = 3
  else
    opt.laststatus = 2
  end
end)

-- recenter / redraw
set('n', '<C-l>', 'zz')
set('n', '<C-u><C-l>', 'zt')

-- " git
-- noremap <silent> gb :Git blame<CR>
-- noremap <silent> gs :Git<CR>

-- " alternates
-- nnoremap <Leader><Tab>  :A<CR>
