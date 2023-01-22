local api = vim.api
local group = api.nvim_create_augroup('TabStack', { clear = true })
local stack = {}

local peek = function()
  return stack[#stack]
end

local push = function(x)
  if x ~= peek() then
    table.insert(stack, x)
  end
end

local pop = function()
  return table.remove(stack)
end

-- NOTE: This syntax declares the local function goto_previous before defining
-- it, which is necessary due to the recursive call.
local function goto_previous()
  local tag = pop()
  if tag then
    if api.nvim_tabpage_is_valid(tag) then
      api.nvim_set_current_tabpage(tag)
    else
      pop()
      goto_previous()
    end
  end
end

tabstack_init = function()
  api.nvim_create_autocmd('TabLeave', {
    group = group,
    callback = function()
      push(api.nvim_get_current_tabpage())
    end,
  })
  api.nvim_create_autocmd('TabClosed', {
    group = group,
    callback = function()
      pop()
      goto_previous()
    end,
  })
end

if vim.v.vim_did_enter == 1 then
  tabstack_init()
else
  -- Defer initializing tabstack until Vim finishes loading. If Vim was invoked
  -- in a way that creates multiple tabpages, it will switch between tabs during
  -- startup, and we don't want those navigations on our stack.
  api.nvim_create_autocmd('VimEnter', {
    group = group,
    callback = tabstack_init,
  })
end
