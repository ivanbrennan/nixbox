local opt = vim.opt

opt.backup = true
opt.backupdir = vim.fn.stdpath('state') .. "/backup//"
opt.backupdir = vim.opt.backupdir + '.' -- fallback
opt.clipboard = 'unnamedplus'
-- opt.cmdheight = 0 -- still experimental
opt.cursorline = true
opt.diffopt = opt.diffopt + 'foldcolumn:0'
opt.expandtab = true
opt.fillchars = { eob = " " }
opt.foldenable = false
opt.foldlevelstart = 10
opt.foldmethod = "syntax"
opt.formatoptions:append('r')
opt.formatoptions:remove('t')
opt.guifont = { "Source Code Pro:h14" }
opt.ignorecase = true
opt.laststatus = 3
opt.lazyredraw = true -- problematic for hydra cmdline
opt.list = true
opt.listchars = { tab = "▸ ", trail = "·", extends = "»", precedes = "«" }
opt.matchtime = 2
opt.mouse = 'a'
opt.nrformats = { 'unsigned' }
opt.number = true
opt.scrolloff = 1

-- Defaults:
--   Neovim: !,'100,<50,s10,h
--
-- - ! save/restore global variables (only all-uppercase variables)
-- - '100 save/restore marks from last 100 files
-- - <50 save/restore 50 lines from each register
-- - s10 max item size 10KB
-- - h do not save/restore 'hlsearch' setting
--
-- Our overrides:
-- - '0 store marks for 0 files
-- - <0 don't save registers
-- - f0 don't store file marks
--
opt.shada = "'0,<0,f0,s10,h"

opt.shiftwidth = 2
opt.shortmess:append('I')
opt.showcmd = true
opt.showmatch = true
opt.showmode = false
opt.showtabline = 0
opt.sidescroll = 2
opt.smartcase = true
opt.softtabstop = -1 -- use shiftwidth
opt.splitright = true
opt.synmaxcol = 256
opt.tabstop = 2
opt.tags:append('./tags;~') -- search for tags recursively upwards until ~
opt.tags:remove('./tags;')  -- don't just look in the current buffer's directory
opt.termguicolors = true
opt.ttimeoutlen = 5
opt.undofile = true
opt.wildignore:append({ '*.class' })
opt.wildignore:append({ '*.gem' })
opt.wildignore:append({ '*.o' })
opt.wildignore:append({ '*.obj' })
opt.wildignore:append({ '*.out' })
opt.wildignore:append({ '*.rar' })
opt.wildignore:append({ '*.swp' })
opt.wildignore:append({ '*.tar.bz2' })
opt.wildignore:append({ '*.tar.gz' })
opt.wildignore:append({ '*.tar.xz' })
opt.wildignore:append({ '*.zip' })
opt.wildignore:append({ '*/.sass-cache/*' })
opt.wildignore:append({ '*/.vagrant/*' })
opt.wildignore:append({ '*/tmp/cache/assets/*/sass/*' })
opt.wildignore:append({ '*/tmp/cache/assets/*/sprockets/*' })
opt.wildignore:append({ '*/vendor/cache/*' })
opt.wildignore:append({ '*~' })
opt.wildignore:append({ '._*' })
opt.wildignore:append({ '.svn' })
opt.wildignore:append({ 'GPATH' })
opt.wildignore:append({ 'GRTAGS' })
opt.wildignore:append({ 'GTAGS' })
opt.wildignorecase = true
opt.wildmode = { 'longest:full', 'full' }
opt.wildoptions = { 'pum' }
opt.wrap = false

local api = vim.api
local cmd = vim.cmd
local opt_local = vim.opt_local

local formattingGroup = api.nvim_create_augroup("Formatting", { clear = true })
api.nvim_create_autocmd("FileType", {
  group = formattingGroup,
  callback = function()
    local tw = vim.o.textwidth
    opt_local.formatprg = "par -w" .. (tw > 0 and tw or 80)
  end
})

local termGroup = api.nvim_create_augroup("TermGroup", { clear = true })
api.nvim_set_hl(0, "EmbeddedTerminal", {
  bg = "#181818"
})
api.nvim_create_autocmd("TermOpen", {
  group = termGroup,
  callback = function()
    opt_local.number = false
    opt_local.cursorline = false
    opt_local.winhighlight = 'Normal:EmbeddedTerminal'
    cmd.startinsert()
  end
})

local g = vim.g

-- sort directory listings like `ls -l`
g.dirvish_mode = [[:sort ir ,/\.\?\zs[^/]\+/\?$,]]

-- Normal colors
g.terminal_color_0  = '#242e38' -- black
g.terminal_color_1  = '#de4d3a' -- red
g.terminal_color_2  = '#b8e068' -- green
g.terminal_color_3  = '#ebcb8b' -- yellow
g.terminal_color_4  = '#a1b3c9' -- blue
g.terminal_color_5  = '#617fa0' -- magenta
g.terminal_color_6  = '#69d2e7' -- cyan
g.terminal_color_7  = '#ffffff' -- white
-- Bright colors
g.terminal_color_8  = '#303d4b' -- black
g.terminal_color_9  = '#f4718c' -- red
g.terminal_color_10 = '#d8e778' -- green
g.terminal_color_11 = '#f7c352' -- yellow
g.terminal_color_12 = '#a8d0e0' -- blue
g.terminal_color_13 = '#53769d' -- magenta
g.terminal_color_14 = '#26a6a6' -- cyan
g.terminal_color_15 = '#ffffff' -- white

cmd.colorscheme('wool')

cmd.aunmenu({ 'PopUp.How-to\\ disable\\ mouse' })
cmd.aunmenu({ 'PopUp.-1-' })
