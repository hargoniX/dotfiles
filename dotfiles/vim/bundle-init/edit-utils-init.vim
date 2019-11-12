" automatically open netrw when no file is specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | Explore | endif

" open netrw on Ctrl + N
map <C-n> :Vexplore<CR>

" Tagbar
map <C-t> :TagbarToggle<CR>

" git gutter
autocmd BufWritePost * GitGutter

let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
