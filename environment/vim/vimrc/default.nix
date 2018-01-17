let
  vimrc = builtins.readFile ./vimrc;
  autocmd = builtins.readFile ./autocmd.vim;
  cursor = builtins.readFile ./cursor.vim;
  filetype = builtins.readFile ./filetype.vim;
  functions = builtins.readFile ./functions.vim;
  keybindings = builtins.readFile ./keybindings.vim;
  netrw = builtins.readFile ./netrw.vim;
  options = builtins.readFile ./options.vim;
  statusline = builtins.readFile ./statusline.vim;
  terminal = builtins.readFile ./terminal.vim;
  wild = builtins.readFile ./wild.vim;
in

''
  runtime vimrc-before.vim
  ${vimrc}
  ${autocmd}
  ${cursor}
  ${filetype}
  ${functions}
  ${keybindings}
  ${netrw}
  ${options}
  ${statusline}
  ${terminal}
  ${wild}
  runtime vimrc-after.vim
''
