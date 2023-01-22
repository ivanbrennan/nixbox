local M = {}

local api = vim.api
local bo = vim.bo
local fn = vim.fn

local excluded_filetypes = { 'qf' }
local excluded_buftypes = { 'terminal' }
local zooms = {}

local get_current_zoom_pair = function()
  local tab = api.nvim_get_current_tabpage()

  for z, w in pairs(zooms) do
    if api.nvim_win_get_tabpage(w) == tab then
      return z, w
    end
  end
end

function M.toggle()
  local filetype = bo.filetype

  for i=1, #excluded_filetypes do
    if filetype == excluded_filetypes[i] then return end
  end
  local buftype = bo.buftype
  for i=1, #excluded_buftypes do
    if buftype == excluded_buftypes[i] then return end
  end

  local zwin, win = get_current_zoom_pair()

  if zwin then
    if api.nvim_win_is_valid(zwin) then
      api.nvim_win_close(zwin, false)
      if api.nvim_win_is_valid(win) then
        api.nvim_set_current_win(win)
      end
    end
    zooms[zwin] = nil
  elseif #fn.tabpagebuflist() > 1 then
    local editor = api.nvim_list_uis()[1]
    win = api.nvim_get_current_win()
    zwin = api.nvim_open_win(0, true, {
      relative = 'editor',
      row = 0,
      col = 0,
      height = editor.height,
      width = editor.width,
      zindex = 5,
    })
    zooms[zwin] = win
    api.nvim_win_set_option(zwin, 'winhighlight', 'Normal:Normal,NormalFloat:Normal')
  end
end

local group = api.nvim_create_augroup("Zoom", { clear = true })
api.nvim_create_autocmd("VimResized", {
  group = group,
  callback = function()
    local editor = api.nvim_list_uis()[1]
    for zwin, _win in pairs(zooms) do
      if api.nvim_win_is_valid(zwin) then
        api.nvim_win_set_config(zwin, {
          height = editor.height,
          width = editor.width,
        })
      end
    end
  end
})
-- buffer change (e.g. bnext)
-- QuickFixCmdPre
-- WinLeave
-- WinNew
-- BufWinLeave

return M
