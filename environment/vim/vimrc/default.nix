let
  vimrc = builtins.readFile ./vimrc;
  autocmd = builtins.readFile ./autocmd.vim;
  filetype = builtins.readFile ./filetype.vim;
  functions = builtins.readFile ./functions.vim;
  keybindings = builtins.readFile ./keybindings.vim;
  netrw = builtins.readFile ./netrw.vim;
  options = builtins.readFile ./options.vim;
  terminal = builtins.readFile ./terminal.vim;
  wild = builtins.readFile ./wild.vim;
in

''
  ${vimrc}
  ${keybindings}
  ${autocmd}
  ${filetype}
  ${functions}
  ${netrw}
  ${options}
  ${terminal}
  ${wild}
''
