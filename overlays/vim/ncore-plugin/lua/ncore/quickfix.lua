local bo = vim.bo
local cmd = vim.cmd
local fn = vim.fn
local create_augroup = vim.api.nvim_create_augroup
local create_autocmd = vim.api.nvim_create_autocmd

local in_quickfix = function()
  return bo.filetype == 'qf'
end

local just_quickfix = function()
  return in_quickfix() and fn.winnr('$') == 1
end

local group = create_augroup('ncore_quickfix', { clear = true })

create_autocmd('QuitPre', {
  group = group,
  callback = function()
    if not in_quickfix() then cmd('silent! lclose') end
  end,
})
create_autocmd('WinEnter', {
  group = group,
  callback = function()
    if just_quickfix() then cmd.quit() end
  end,
})

local quickfix = {}

local loclist_exists = function()
  return fn.getloclist(0, { id = 0 }).id ~= 0
end

local loclist_is_open = function()
  return fn.getloclist(0, { winid = 0 }).winid ~= 0
end

local quickfix_is_open = function()
  return fn.getqflist({ winid = 0 }).winid ~= 0
end

quickfix.toggle_loclist = function()
  if loclist_is_open() then
    cmd.lclose()
  else
    cmd('silent! lopen')
  end
end

quickfix.toggle_quickfix = function()
  if quickfix_is_open() then
    cmd.cclose()
  else
    cmd('botright copen')
  end
end

local getlist = function(prefix)
  if prefix == 'l' then
    return fn.getloclist(0, { context = 0 })
  else
    return fn.getqflist({ context = 0 })
  end
end

local getquery = function(prefix)
  local ctx = getlist(prefix).context
  return type(ctx) == type({}) and ctx.query or ''
end

local recall_search_pattern = function(prefix)
  local qry = getquery(prefix)
  if #qry > 0 then fn.setreg('/', qry) end
end

local unsafe_go = function(prefix, command)
  cmd(prefix..command)
  cmd('silent doautocmd <nomodeline> User ncore_quickfix_'..command)
  recall_search_pattern(prefix)
end

local go = function(command)
  local prefix = loclist_exists() and 'l' or 'c'
  local ok, err = pcall(unsafe_go, prefix, command)

  if ok then
    return -- Success.
  end
  if err:match(':E42:') or err:match(':E553:') then
    return -- No items, or no more items.
  end
  if err:match(':E380:') or err:match(':E381:') then
    return -- No more lists.
  end

  error(err)
end

quickfix.toggle = function()
  if loclist_exists() then
    quickfix.toggle_loclist()
  else
    quickfix.toggle_quickfix()
  end
end

quickfix.next = function() go('next') end
quickfix.next_file = function() go('nfile') end

quickfix.previous = function() go('prev') end
quickfix.previous_file = function() go('pfile') end

quickfix.newer = function() go('newer') end
quickfix.older = function() go('older') end

return quickfix
