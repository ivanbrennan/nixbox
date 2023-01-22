local set = vim.keymap.set
local api = vim.api
local fn = vim.fn

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
set('n', '<M-CR>', '<Cmd>call append(line("."), "")<CR>')

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

vim.opt.cedit = '<C-o>'

-- + -
set('n', '+', '<C-a>')
set('x', '+', '<C-a>')
set('n', '_', '<C-x>')
set('x', '_', '<C-x>')

-- commentary
set({ 'x', 'n', 'o' }, '<Leader>;', '<Plug>Commentary', { remap = true })
set('n', '<Leader>;;', '<Plug>CommentaryLine', { remap = true })

-- " alternates
-- nnoremap <Leader><Tab>  :A<CR>

set('n', '<M-u>', '<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-l><CR>')
set('n', '<C-d>', '<C-w><C-q>')
set('n', 'U', '<C-r>')
