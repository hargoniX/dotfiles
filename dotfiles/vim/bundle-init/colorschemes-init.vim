" gruvbox
colorscheme gruvbox
set background=dark
let g:airline_theme='gruvbox'
set termguicolors

" line limit column
set colorcolumn=80

" highlight current line
set cursorline

" show whitespaces / tabs
set list
set listchars=tab:•\ ,trail:•,extends:»,precedes:«

" Change cursor shape in different modes
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
