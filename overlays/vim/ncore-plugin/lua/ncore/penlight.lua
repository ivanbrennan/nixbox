local cmd = vim.cmd
local fn = vim.fn

local syntax_name = function(id)
  return fn.synIDattr(id, 'name')
end

penlight = {}

penlight.syntax_ids = function()
  return fn.synstack(fn.line('.'), fn.col('.'))
end

penlight.syntax_names = function()
  local ids = penlight.syntax_ids()
  local names = {}
  for i=1, #ids do
    names[i] = syntax_name(ids[i])
  end
  return names
end

penlight.report = function(arg)
  local ids = penlight.syntax_ids()
  local start = math.max(math.min(arg or 1, #ids), 1)

  print(string.format('"%s"', fn.expand('<cword>')))

  for i=start, #ids do
    cmd('verbose hi ' .. syntax_name(ids[i]))
  end
end

return penlight
