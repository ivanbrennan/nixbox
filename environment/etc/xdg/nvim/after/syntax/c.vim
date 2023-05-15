" Highlight Class and Function names
syntax match cCustomFunc /\w\+\s*(/me=e-1,he=e-1
highlight def link cCustomFunc Function

if !exists("c_no_c99") " ISO C99
  syn keyword cBoolean true false
endif
