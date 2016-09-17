syntax on

" Tab specific option
set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=2               "Insert 4 spaces when tab is pressed
set shiftwidth=2                "An indent is 4 spaces
set shiftround                  "Round indent to nearest shiftwidth multiple
" Add the following to your .vimrc to automatically load this on startup

if filereadable(".vim.custom")
    so .vim.custom
endif

function StripTrailingWhitespace()
  let myline=line(".")
  let mycolumn = col(".")
  silent %s/  *$//
  call cursor(myline, mycolumn)
endfunction
set autoindent
