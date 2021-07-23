" vim: set tw=99 ts=2 sts=2 sw=2 et:

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword cachetTypeDef enum struct impl nextgroup=cachetIdent skipwhite skipempty
syn keyword cachetFnDef op fn nextgroup=cachetFnIdent skipwhite skipempty
syn keyword cachetVarDef const
syn keyword cachetTag fallible unsafe
syn keyword cachetCheck assert guard
syn keyword cachetOperator as
syn keyword cachetKeyword let out

syn match cachetIdent "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match cachetFnIdent "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn keyword cachetBuiltInType Unit Bool Int32 Double
syn keyword cachetUnitVar unit
syn keyword cachetBoolVar true false

syn match cachetPath "\w\(\w\)*::"he=e-2,me=e-2
syn match cachetPathSep "::"

syn match cachetCall "\w\(\w\)*("he=e-1,me=e-1

syn match cachetOperator "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
syn match cachetOperator "<:"
syn match cachetOperator display "&&\|||"

syn region cachetFoldBraces start="{" end="}" transparent fold

syn region cachetLineComment start="//" end="$" contains=cachetTodo,@Spell
" TODO: Add support for nested block comments to the Cachet lexer.
"syn region cachetBlockComment matchgroup=cachetBlockComment start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=cachetTodo,cachetNestedBlockComment,@Spell
"syn region cachetNestedBlockComment matchgroup=cachetBlockComment start="/\*" end="\*/" contains=cachetTodo,cachetNestedBlockComment,@Spell contained transparent
syn region cachetBlockComment matchgroup=cachetBlockComment start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=cachetTodo,@Spell

syn keyword cachetTodo contained TODO FIXME XXX NB NOTE SAFETY

hi def link cachetTypeDef Keyword
hi def link cachetFnDef Keyword
hi def link cachetVarDef Keyword
hi def link cachetTag Exception
hi def link cachetCheck Exception
hi def link cachetKeyword Keyword

hi def link cachetIdent Identifier
hi def link cachetFnIdent Function

hi def link cachetBuiltInType Type
hi def link cachetUnitVar Constant
hi def link cachetBoolVar Boolean

hi def link cachetPath Include
hi def link cachetPathSep Delimiter

hi def link cachetCall Function

hi def link cachetOperator Operator

hi def link cachetLineComment Comment
hi def link cachetBlockComment Comment

hi def link cachetTodo Todo

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "cachet"
