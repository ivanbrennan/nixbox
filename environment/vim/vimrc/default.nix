let
  vimrc = builtins.readFile ./vimrc;
  autocmd = builtins.readFile ./autocmd.vim;
  colorscheme = builtins.readFile ./colorscheme.vim;
  cursor = builtins.readFile ./cursor.vim;
  filetype = builtins.readFile ./filetype.vim;
  functions = builtins.readFile ./functions.vim;
  keybindings = builtins.readFile ./keybindings.vim;
  netrw = builtins.readFile ./netrw.vim;
  options = builtins.readFile ./options.vim;
  terminal = builtins.readFile ./terminal.vim;
  wild = builtins.readFile ./wild.vim;
in

''
  runtime vimrc-before.vim
  ${vimrc}
  ${autocmd}
  ${colorscheme}
  ${cursor}
  ${filetype}
  ${functions}
  ${keybindings}
  ${netrw}
  ${options}
  ${terminal}
  ${wild}
  runtime vimrc-after.vim
''
