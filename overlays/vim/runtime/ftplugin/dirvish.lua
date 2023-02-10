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

set('n', 'q', '<Plug>(dirvish_quit)', {
  buffer = true,
  remap  = true,
  nowait = true,
})

-- For some reason, conceallevel=3 only takes effect if we run these commands
-- in-line, rather than performing them within a function.
local conceal_script = table.concat({
  ':let b:saved_position=getcurpos()<Bar>',
  [[silent keeppatterns g@\v/\.[^\/]+/?$@d _<CR>]],
  ':setlocal conceallevel=3<Bar>',
  'let b:showing_hidden_files=0<Bar>',
  'call setpos(".", b:saved_position)<Bar>',
  'unlet b:saved_position<CR>',
})
set('n', '<Plug>(conceal_hidden_files)', conceal_script, {
  buffer = true,
  silent = true,
})

set('n', '<Plug>(reveal_hidden_files)', function()
  local saved_position = fn.getcurpos()
  cmd.Dirvish()
  b.showing_hidden_files = 1
  fn.setpos(".", saved_position)
end, { buffer = true, silent = true })

set('n', 'h', function()
  if b.showing_hidden_files == 1 then
    return '<Plug>(conceal_hidden_files)'
  else
    return '<Plug>(reveal_hidden_files)'
  end
end, { buffer = true, expr = true })
