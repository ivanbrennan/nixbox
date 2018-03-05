" ::::::::: Colors ::::::::::::::::::::::::

if has('termguicolors')
  set termguicolors
endif

if !exists('g:colors_name')
  if $THEME == 'light'
    colorscheme blight
  else
    colorscheme root
  endif
endif
