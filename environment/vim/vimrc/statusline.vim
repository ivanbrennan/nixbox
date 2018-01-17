set laststatus=2

setg statusline=\ 

setg statusline+=%f    " relative path

setg statusline+=\ 

setg statusline+=%y    " filetype (current)

setg statusline+=\ 

setg statusline+=%w    " preview
setg statusline+=%M    " modified

setg statusline+=%=    " separator

setg statusline+=\ 

setg statusline+=%l:   " line:
setg statusline+=%v    " column

setg statusline+=\ 
