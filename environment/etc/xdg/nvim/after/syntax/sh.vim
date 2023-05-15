" Recognize functions nested within an { expression-group; }
syn cluster shExprList2 add=shFunctionOne,shFunctionTwo,shFunctionThree,shFunctionFour

" Allow { expression-group; } to fold
syn region shExpr transparent matchgroup=shExprRegion start="{" end="}" contains=@shExprList2 nextgroup=shSpecialNxt fold

" Extensions for Bats (Bash Automated Testing System)
syn match batsTest       "\v\@test"
syn keyword batsKeyword  run containedin=shExpr contained

hi def link batsTest     Identifier
hi def link batsKeyword  Keyword
