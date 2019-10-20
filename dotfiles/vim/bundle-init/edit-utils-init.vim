" automatically open NERDTree when no file is specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" Automatically open NerdTREE for directories
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif

" open NERDTree on Ctrl + N
map <C-n> :NERDTreeToggle<CR>

" Tagbar
map <C-t> :TagbarToggle<CR>

" git gutter
autocmd BufWritePost * GitGutter
