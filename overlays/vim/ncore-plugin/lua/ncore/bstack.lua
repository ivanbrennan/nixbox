local cmd = vim.cmd
local fn = vim.fn

local bstack = {}

bstack.quit = function()
  if fn.winnr('$') > 1 then
    cmd.close()
  else
    local alt = fn.bufnr('#')
    if alt ~= -1 then
      cmd.buffer(alt)
    else
      cmd.quit()
    end
  end
end

return bstack
