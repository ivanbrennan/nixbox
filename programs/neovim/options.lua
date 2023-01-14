--[[ Defaults as of 9220755302317e8030c5bbf334357c0d64df9fa4
vim.api.nvim_command('filetype plugin indent on')
vim.opt.autoindent = true
vim.opt.autoread = true
vim.opt.background = "dark"
vim.opt.backspace = "indent,eol,start"
vim.opt.belloff = "all"
vim.opt.compatible = false
vim.opt.complete = ".,w,b,u,t"
vim.opt.directory = vim.fn.stdpath('state') .. "/swap//"
vim.opt.display = "lastline"
vim.opt.encoding = "utf-8"
vim.opt.fsync = false
vim.opt.hidden = true
vim.opt.history = 10000
vim.opt.incsearch = true
vim.opt.joinspaces = false
vim.opt.langnoremap = true
vim.opt.langremap = false
vim.opt.laststatus = 2
vim.opt.listchars = { tab = "> ", trail = "-", nbsp = "+" }
vim.opt.mousemodel = "popup_setpos"
vim.opt.nrformats = "bin,hex"
vim.opt.ruler = true
vim.opt.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,terminal"
vim.opt.showcmd = true
vim.opt.smarttab = true
vim.opt.startofline = false
vim.opt.switchbuf = "uselast"
vim.opt.tabpagemax = 50
vim.opt.timeout = true
vim.opt.timeoutlen = 1000
vim.opt.ttyfast = true
vim.opt.undodir = vim.fn.stdpath('state') .. "/undo//"
vim.opt.viewoptions = "folds,cursor,curdir"
vim.opt.wildmenu = true
vim.opt.wildoptions = "pum,tagfile"
--]]

vim.opt.backupdir = vim.fn.stdpath('state') .. "/backup//"
vim.opt.fillchars = { eob = " " }
vim.opt.formatoptions:remove('t') -- don't auto-wrap non-commented text
vim.opt.formatoptions:append('r') -- auto-comment with Enter
vim.opt.number = true
vim.opt.mouse = 'a'
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false
vim.opt.wrap = false
vim.opt.sidescroll = 2
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.shortmess:append('I') -- hide the intro message
vim.opt.tags:remove('./tags')   -- don't just look in the current buffer's directory (vim)
vim.opt.tags:remove('./tags;')  -- don't just look in the current buffer's directory (nvim)
vim.opt.tags:append('./tags;~') -- search for tags recursively upwards until ~
vim.opt.ttimeoutlen = 5

vim.opt.termguicolors = true
vim.cmd('colorscheme wool')
-- :help nvim_set_hl()
-- vim.api.nvim_set_hl(0, 'WinSeparator', { fg = '#2c2f30', bold = true })

vim.cmd.aunmenu({ 'PopUp.How-to\\ disable\\ mouse' })
vim.cmd.aunmenu({ 'PopUp.-1-' })

--[[ Defaults
vim.keymap.set('n', 'Y', 'y$')
vim.keymap.set('i', '<C-U>', '<C-G>u<C-U>')
vim.keymap.set('i', '<C-W>', '<C-G>u<C-W>')
vim.keymap.set('x', '*', 'y/\V<C-R>"<CR>')
vim.keymap.set('x', '#', 'y?\V<C-R>"<CR>')
vim.keymap.set('n', '&', ':&&<CR>')
--]]

-- We can now map Tab and C-i separately, but what should we map Tab to?
vim.keymap.set('n', '<C-i>', '<C-i>')
vim.keymap.set('n', '<Tab>', '<Cmd>bnext<CR>')
vim.keymap.set('n', '<S-Tab>', '<Cmd>bprevious<CR>')

vim.g.mapleader = ' '
vim.keymap.set('n', '<M-u>', '<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>')
vim.keymap.set('n', '<leader>.', ":edit <C-R>=empty(expand('%')) ? '' : expand('%:~:.:h').'/'<CR>")
vim.keymap.set('n', '<leader>h', ':help ')
vim.keymap.set('n', '<leader>fs', ':write<CR>')
vim.keymap.set('n', '<C-D>', '<C-W><C-Q>')
vim.keymap.set('n', 'U', '<C-R>')
