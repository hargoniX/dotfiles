set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0


let g:lsc_enable_diagnostics = 1

let g:lsc_server_commands = {
\   "java": {
\       "name": "java",
\       "command": "java_lsp.sh --quiet",
\       "suppress_stderr": v:true
\   },
\   "python": {
\       "name": "pyls",
\       "command": "pyls",
\       "supress_stderr": v:true
\   },
\   "rust": {
\       "name": "rls",
\       "command": "rls",
\       "supress_stderr": v:true
\   }
\}
