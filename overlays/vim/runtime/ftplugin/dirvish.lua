b = vim.b
cmd = vim.cmd
fn = vim.fn
set = vim.keymap.set

-- reset this even if reloading
b.show_hidden_files = 1

-- finish here if reloading
if b.loaded_user_ftplugin == 1 then
  return
end
b.loaded_user_ftplugin = 1

vim.opt_local.number = false

set('n', 'm', '<CR>', { buffer = true, remap  = true })

set('n', 'q', '<Plug>(dirvish_quit)', {
  buffer = true,
  remap  = true,
  nowait = true,
})

set('n', 'h', function()
  local cursor_position = fn.getcurpos()

  if b.show_hidden_files == 1 then
    cmd([[keeppatterns g@\v/\.[^\/]+/?$@d]])
    b.show_hidden_files = 0
  else
    cmd.Dirvish('%')
    b.show_hidden_files = 1
  end

  fn.setpos('.', cursor_position)
end, { buffer = true, silent = true })
