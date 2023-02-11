local api = vim.api
local fn = vim.fn
local g = vim.g
local o = vim.o

local current = function()
  local b = g.actual_curbuf
  return b and tonumber(b) == api.nvim_get_current_buf()
end

local is_prompt = function()
  return o.buftype == 'prompt'
end

local get_dir = function()
  if #fn.bufname('%') > 0 then
    local tree = fn.FugitiveWorkTree()
    return #tree > 0 and (fn.fnamemodify(tree, ':t') .. '/') or ''
  else
    return ''
  end
end

local get_bufname = function()
  local bufname = fn.bufname('%')
  local tree = fn.FugitiveWorkTree()

  if #bufname > 0 and #tree > 0 then
    return string.sub(fn.FugitivePath(), #tree + 2)
  else
    return bufname
  end
end

mline_dir = function()
  return current() and get_dir() or ''
end

mline_dir_nc = function()
  return not current() and get_dir() or ''
end

mline_bufname = function()
  return current() and get_bufname() or ''
end

mline_bufname_nc = function()
  return not current() and get_bufname() or ''
end

mline_basic_bufname = function()
  return current() and fn.bufname('%') or ''
end

mline_basic_bufname_nc = function()
  return not current() and fn.bufname('%') or ''
end

mline_filetype = function()
  return current() and not is_prompt() and o.filetype or ''
end

mline_filetype_nc = function()
  return not current() and not is_prompt() and o.filetype or ''
end

mline_before_filetype = function()
  return #o.filetype > 0 and not is_prompt() and '[' or ''
end

mline_after_filetype = function()
  return #o.filetype > 0 and not is_prompt() and ']' or ''
end

mline_modified = function()
  if is_prompt() then
    return ''
  elseif not o.modifiable then
    return '-'
  elseif o.modified then
    return '+'
  else
    return ''
  end
end

g.mline_branch_maxwidth = g.mline_branch_maxwidth or 20

mline_branch = function()
  local name = fn.FugitiveHead()

  if #name > 0 then
    local maxwidth = g.mline_branch_maxwidth or 0
    if maxwidth > 0 and #name > maxwidth then
      name = string.sub(name, 1, maxwidth - 1) .. '‥'
    end
    return string.format('(%s)', name)
  else
    return ''
  end
end

mline_tabdot = function()
  return fn.tabpagenr('$') > 1 and '· ' or ' '
end

local statusline = function()
  local fugitive = g.loaded_fugitive
  -- TODO: try converting to string.format() for performance
  return table.concat({
    ' ',
    '%1*',                                -- User1 highlight group (dirname)
    fugitive and '%{v:lua.mline_dir()}' or '', -- worktree
    '%2*',                                -- User2 highlight group (filename)
    fugitive and '%{v:lua.mline_bufname()}' or '%{v:lua.mline_basic_bufname()}', -- relative path
    '%*',                                 -- reset highlight group
    fugitive and '%{v:lua.mline_dir_nc()}' or '', -- worktree (non-current)
    fugitive and '%{v:lua.mline_bufname_nc()}' or '%{v:lua.mline_basic_bufname_nc()}', -- relative path (non-current)
    ' ',
    '%#StatusLineNC#',                    -- StatusLineNC highlight group
    '%{v:lua.mline_before_filetype()}',   -- dimmed '['
    '%3*',                                -- User3 highlight group (filetype)
    '%{v:lua.mline_filetype()}',          -- filetype (current)
    '%*',                                 -- reset highlight group
    '%{v:lua.mline_filetype_nc()}',       -- filetype (non-current)
    '%#StatusLineNC#',                    -- StatusLineNC highlight group
    '%{v:lua.mline_after_filetype()}',    -- dimmed ']'
    '%*',                                 -- reset highlight group
    ' ',
    '%w',                                 -- preview
    '%{v:lua.mline_modified()}',          -- modified
    '%=',                                 -- separator
    ' ',
    '%{toupper(&fenc)}',                  -- encoding
    fugitive and '%(  %{v:lua.mline_branch()}%)' or '', -- branch
    ' ',
    '%{v:lua.mline_tabdot()}',            -- tabs indicator
    '%l:',                                -- line:
    '%#StatusLineNC#',                    -- dim
    '%v',                                 -- column
    '%*',                                 -- reset highlight group
    ' ',
  })
end

mline_update_highlight = function()
  local ok1, stat = pcall(api.nvim_get_hl_by_name, 'StatusLine', true)
  if not ok1 then return end

  local ok2, cmt = pcall(api.nvim_get_hl_by_name, 'Comment', true)
  if not ok2 then return end

  local bg = stat.background
  local fg = stat.foreground
  local cmt_fg = cmt.foreground

  if o.modified then
    api.nvim_set_hl(0, 'User1', { bg = bg, fg = fg, italic = true, bold = true })
    api.nvim_set_hl(0, 'User2', { bg = bg, fg = fg, italic = true, bold = true })
    api.nvim_set_hl(0, 'User3', { bg = bg, fg = fg, italic = true })
  else
    api.nvim_set_hl(0, 'User1', { bg = bg, fg = cmt_fg, bold = true })
    api.nvim_set_hl(0, 'User2', { bg = bg, fg = fg, bold = true })
    api.nvim_set_hl(0, 'User3', { bg = bg, fg = fg })
  end

  -- Unconditional italics, for use in the Quickfix statusline.
  api.nvim_set_hl(0, 'User4', { bg = bg, fg = fg, italic = true })
end

local highlight_modified = false

mline_check_modified = function()
  local modified = o.modified

  if modified and not highlight_modified then
    highlight_modified = true
    mline_update_highlight()
  elseif not modified and highlight_modified then
    highlight_modified = false
    mline_update_highlight()
  end
end

local init_statusline = function()
  vim.opt_global.statusline = statusline()
  vim.cmd.redrawstatus()
  mline_update_highlight()
end

local init_autocommands = function()
  local mline_group = api.nvim_create_augroup('Mline', {
    clear = true
  })
  api.nvim_create_autocmd('ColorScheme', {
    group = mline_group,
    callback = mline_update_highlight,
  })
  api.nvim_create_autocmd({
    'BufWinEnter',
    'BufWritePost',
    'FileWritePost',
    'TextChanged',
    'TextChangedI',
    'WinEnter'
  }, {
    group = mline_group,
    callback = mline_check_modified,
  })
end

local mline_init = function()
  init_statusline()
  init_autocommands()
end

if vim.v.vim_did_enter == 1 then
  mline_init()
else
  -- Defer initializing mline until Vim finishes loading startup scripts.
  -- This allows for a colorscheme and any dependent plugins to load first.
  local mline_group = api.nvim_create_augroup('Mline', {
    clear = true
  })
  api.nvim_create_autocmd('VimEnter', {
    group = mline_group,
    callback = mline_init,
  })
end
