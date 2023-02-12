local bo = vim.bo
local fn = vim.fn

local finishing_word = function(line, col)
  -- preceded by word/filename char AND NOT inside word
  local prev = line:sub(col-1, col-1)
  local char = line:sub(col, col)
  return fn.match(prev, '\\k\\|[/~.]') ~= -1 and fn.match(char, '\\k') == -1
end

local want_tab = function()
  -- this needs to be smarter
  return not bo.expandtab
end

local complete = function(direction)
  return direction > 0 and '<C-n>' or '<C-p>'
end

local indent = function()
  return (#bo.indentexpr > 0 or bo.cindent) and '<C-f>' or '<Tab>'
end

local tabkey = {}

tabkey.complete_or_indent = function(direction)
  -- complete if popup-menu displayed
  if fn.pumvisible() > 0 then return complete(direction) end

  local line = fn.getline('.') -- current line
  local col  = fn.col('.')     -- current column

  if direction > 0 and (not finishing_word(line, col) or want_tab()) then
    return indent()
  end

  -- non-whitespace characters before cursor
  local prefix = fn.expand(fn.matchstr(line:sub(1, col-1), [[\S*$]]), 1)

  -- complete filename if finishing a path
  if prefix:match('^[~/.]') then return '<C-x><C-f>' end

  -- perform custom completion if possible
  if #bo.completefunc > 0 and fn.call(bo.completefunc, {1, prefix}) >= 0 then
    return '<C-x><C-u>'
  end

  return complete(direction)
end

return tabkey
