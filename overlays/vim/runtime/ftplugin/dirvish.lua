local b = vim.b
local cmd = vim.cmd
local fn = vim.fn
local set = vim.keymap.set

-- reset this even if reloading
b.showing_hidden_files = 1

-- finish here if reloading
if b.loaded_user_ftplugin == 1 then
  return
end
b.loaded_user_ftplugin = 1

vim.opt_local.number = false

set('n', 'm', '<CR>', { buffer = true, remap  = true })

set('n', '<Tab>', '<Plug>(dirvish_arg)', { buffer = true, remap  = true })

set('n', 'q', '<Plug>(dirvish_quit)', {
  buffer = true,
  remap  = true,
  nowait = true,
})

set('n', '<Plug>(toggle_hidden_files)', function()
  local position = fn.getcurpos()

  if b.showing_hidden_files == 1 then
    cmd([[silent keeppatterns g@\v/\.[^\/]+/?$@d _]])
    b.showing_hidden_files = 0
  else
    cmd.Dirvish()
    b.showing_hidden_files = 1
  end

  fn.setpos('.', position)
end, { buffer = true })

-- Due to a TextChanged autocommand that dirvish sets up, it's necessary to set
-- conceallevel in a subsequent command, rather than attempting to do so in the
-- same command that modifies the dirvish buffer contents.
-- https://github.com/justinmk/vim-dirvish/blob/6233243f0caa71d27d27ea102540a88bce8eb6ea/autoload/dirvish.vim#L184-L185
set('n', '<Plug>(conceallevel3)', '<Cmd>setlocal conceallevel=3<CR>', { buffer = true })

set('n', 'h', '<Plug>(toggle_hidden_files)<Plug>(conceallevel3)', { buffer = true, remap = true })
