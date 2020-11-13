" nvim <> vim compatability layer
set runtimepath+=~/.vim runtimepath+=~/.vim/after runtimepath+=~/.config/nvim/lua
let &packpath = &runtimepath
source ~/.vimrc
