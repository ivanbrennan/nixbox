vim.opt.backup = true
vim.opt.backupdir = vim.fn.stdpath('state') .. "/backup//"
vim.opt.clipboard = 'unnamedplus'
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.fillchars = { eob = " " }
vim.opt.foldenable = false
vim.opt.foldlevelstart = 10
vim.opt.foldmethod = "syntax"
vim.opt.formatoptions:append('r')
vim.opt.formatoptions:remove('t')
vim.opt.guifont = { "Source Code Pro:h14" }
vim.opt.hlsearch = false
vim.opt.ignorecase = true
vim.opt.lazyredraw = true
vim.opt.list = true
vim.opt.listchars = { tab = "▸ ", trail = "·", extends = "»", precedes = "«" }
vim.opt.matchtime = 2
vim.opt.mouse = 'a'
vim.opt.nrformats = { 'unsigned' }
vim.opt.number = true
vim.opt.scrolloff = 1
vim.opt.shiftwidth = 2
vim.opt.shortmess:append('I')
vim.opt.showmatch = true
vim.opt.showmode = false
vim.opt.sidescroll = 2
vim.opt.smartcase = true
vim.opt.softtabstop = 2
vim.opt.synmaxcol = 256
vim.opt.tabstop = 2
vim.opt.tags:append('./tags;~') -- search for tags recursively upwards until ~
vim.opt.tags:remove('./tags;')  -- don't just look in the current buffer's directory
vim.opt.termguicolors = true
vim.opt.ttimeoutlen = 5
vim.opt.undofile = true
vim.opt.wildignore:append({ '*.class' })
vim.opt.wildignore:append({ '*.gem' })
vim.opt.wildignore:append({ '*.o' })
vim.opt.wildignore:append({ '*.obj' })
vim.opt.wildignore:append({ '*.out' })
vim.opt.wildignore:append({ '*.rar' })
vim.opt.wildignore:append({ '*.swp' })
vim.opt.wildignore:append({ '*.tar.bz2' })
vim.opt.wildignore:append({ '*.tar.gz' })
vim.opt.wildignore:append({ '*.tar.xz' })
vim.opt.wildignore:append({ '*.zip' })
vim.opt.wildignore:append({ '*/.sass-cache/*' })
vim.opt.wildignore:append({ '*/.vagrant/*' })
vim.opt.wildignore:append({ '*/tmp/cache/assets/*/sass/*' })
vim.opt.wildignore:append({ '*/tmp/cache/assets/*/sprockets/*' })
vim.opt.wildignore:append({ '*/vendor/cache/*' })
vim.opt.wildignore:append({ '*~' })
vim.opt.wildignore:append({ '._*' })
vim.opt.wildignore:append({ '.svn' })
vim.opt.wildignore:append({ 'GPATH' })
vim.opt.wildignore:append({ 'GRTAGS' })
vim.opt.wildignore:append({ 'GTAGS' })
vim.opt.wildignorecase = true
vim.opt.wildmode = { 'longest:full', 'full' }
vim.opt.wildoptions = { 'pum' }
vim.opt.wrap = false

local formattingGroup = vim.api.nvim_create_augroup("Formatting", {
  clear = true
})
vim.api.nvim_create_autocmd("FileType", {
  group = formattingGroup,
  callback = function()
    local tw = vim.o.textwidth
    vim.opt_local.formatprg = "par -w" .. (tw > 0 and tw or 80)
  end
})

local termGroup = vim.api.nvim_create_augroup("TermGroup", {
  clear = true
})
vim.api.nvim_set_hl(0, "EmbeddedTerminal", {
  bg = "#181818"
})
vim.api.nvim_create_autocmd("TermOpen", {
  group = termGroup,
  callback = function()
    vim.opt_local.number = false
    vim.opt_local.cursorline = false
    vim.opt_local.winhighlight = 'Normal:EmbeddedTerminal'
    vim.cmd('startinsert')
  end
})

-- Normal colors
vim.g.terminal_color_0  = '#242e38' -- black
vim.g.terminal_color_1  = '#de4d3a' -- red
vim.g.terminal_color_2  = '#b8e068' -- green
vim.g.terminal_color_3  = '#ebcb8b' -- yellow
vim.g.terminal_color_4  = '#a1b3c9' -- blue
vim.g.terminal_color_5  = '#617fa0' -- magenta
vim.g.terminal_color_6  = '#69d2e7' -- cyan
vim.g.terminal_color_7  = '#ffffff' -- white
-- Bright colors
vim.g.terminal_color_8  = '#303d4b' -- black
vim.g.terminal_color_9  = '#f4718c' -- red
vim.g.terminal_color_10 = '#d8e778' -- green
vim.g.terminal_color_11 = '#f7c352' -- yellow
vim.g.terminal_color_12 = '#a8d0e0' -- blue
vim.g.terminal_color_13 = '#53769d' -- magenta
vim.g.terminal_color_14 = '#26a6a6' -- cyan
vim.g.terminal_color_15 = '#ffffff' -- white

vim.cmd('colorscheme wool')

vim.cmd.aunmenu({ 'PopUp.How-to\\ disable\\ mouse' })
vim.cmd.aunmenu({ 'PopUp.-1-' })
