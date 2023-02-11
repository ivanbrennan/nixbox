local api = vim.api
local fn = vim.fn
local g = vim.g

g.tabline_fname_maxwidth = g.tabline_fname_maxwidth or 20

calculate_tabline = function()
  local elems = {}
  local numtabs = fn.tabpagenr('$')
  local current = fn.tabpagenr()
  local maxwidth = g.tabline_fname_maxwidth or 0

  for i=1, numtabs do
    local highlight = i == current and '%#TabLineSel#' or '%#TabLine#'
    local bufs = fn.tabpagebuflist(i)
    local winnr = fn.tabpagewinnr(i)
    local fname = fn.fnamemodify(fn.bufname(bufs[winnr]), ':t')
    if #fname == 0 then
      fname = '◘'
    elseif maxwidth > 0 and #fname > maxwidth then
      fname = string.format('‥%s', string.sub(fname, 1 - maxwidth, -1))
    end
    elems[#elems+1] = string.format('%s%%%dT %s ', highlight, i, fname)
  end
  elems[#elems+1] = '%#TabLineFill#%T'

  return table.concat(elems)
end

local tabline_init = function()
  vim.opt.tabline = '%!v:lua.calculate_tabline()'
  vim.cmd.redrawtabline()
end

if vim.v.vim_did_enter == 1 then
  tabline_init()
else
  -- Defer initializing tabline until Vim finishes loading startup scripts.
  -- This allows for a colorscheme and any dependent plugins to load first.
  local tabline_group = api.nvim_create_augroup('Tabline', {
    clear = true
  })
  api.nvim_create_autocmd('VimEnter', {
    group = tabline_group,
    callback = tabline_init,
  })
end
