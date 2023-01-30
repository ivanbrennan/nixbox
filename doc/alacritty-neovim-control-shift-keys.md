# How to add ctrl-shift mappings in neovim

[original source](https://www.reddit.com/r/neovim/comments/mbj8m5/how_to_setup_ctrlshiftkey_mappings_in_neovim_and/)

Example:
```lua
map('n', '<C-j>', '<cmd>:cn<cr>')
map('n', '<C-k>', '<cmd>:cp<cr>')
map('n', '<C-S-j>', '<cmd>:cnf<cr>')
map('n', '<C-S-k>', '<cmd>:cpf<cr>')
```

The problem is: terminal emulators don't recognize control-shift chords, unless we tell them how to. We'll use alacritty for this example, but most terminal emulators can be configured almost the same way.

The first step is to choose the char we want to map (e.g. `j` if we're mapping `C-S-j`).

Then, lookup the decimal value of that capitalized unicode char (e.g. the decimal value for `J` is 74). Use [the wikipedia page on unicode chars](https://en.wikipedia.org/wiki/List_of_Unicode_characters) as a reference.

Once you've found that value, we need to tell our terminal emulator how to recognize the chord, and translate it into something that neovim can understand.

Add the following line under the mappings section in your `alacritty.yml` config file:
```
  - { key: J, mods: Control|Shift, chars: "\x1b[74;5u" }
```
This means: the char `J`, combined with the mods `Control` and `Shift` (the pipe here is the join char), will translate to the escape sequence `\x1b[74;5u`.

The escape sequence is made of the following parts:

- `\x1b[` is a CSI (control sequence introducer), meaning `ESC+[`;
- `74` is the decimal value of char `J` (see above);
- `5` is the sum of bits for the ctrl and shift modifiers (`1` is `shift`, `2` is `alt`, `4` is `ctrl`);
- `u` stands for unicode.

Save the config file, then add the mapping in your `init.vim` or `init.lua`:
```lua
-- init.lua
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n', '<C-S-j>', '<cmd>:cnf<cr>')
```
Or:
```
nnoremap <C-S-j> :cnf<cr>
```
And that's it!

### Kitty terminal
The syntax used to add a custom mapping in kitty is the following:
```
map ctrl+shift+j send_text all \x1b[74;5u
```

## Sources

- [How to map Ctrl+A and Ctrl+Shift+A differently?](https://stackoverflow.com/questions/1506764/how-to-map-ctrla-and-ctrlshifta-differently)
- [alacritty.yml](https://github.com/alacritty/alacritty/blob/master/alacritty.yml)
- [Fix keyboard input on terminals - Please](http://www.leonerd.org.uk/hacks/fixterms/)
