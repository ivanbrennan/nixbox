local api = vim.api
local cmd = vim.cmd
local fn = vim.fn

local group = api.nvim_create_augroup('NCoreGui', { clear = true })

api.nvim_create_autocmd('UIEnter', {
  group = group,
  callback = function()
    if fn.exists(':GuiAdaptiveColor') == 2 then
      cmd.GuiAdaptiveColor(1)
      if fn.exists(':GuiPopupmenu') == 2 then
        cmd.GuiPopupmenu(1)
      end
    end
  end,
})
