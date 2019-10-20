" Ale
let g:ale_linters = {
\    'python': [],
\    'perl': ['perl'],
\    'zsh': ['shell'],
\}

let g:ale_fixers = {
\    '*': ['remove_trailing_lines', 'trim_whitespace']
\}

let g:ale_sign_column_always = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_enter = 1
let g:ale_fix_on_save = 1
let b:ale_warn_about_trailing_whitespace = 1

" Gruvbox inspired highlighting for ALE
highlight ALEError cterm=underline ctermfg=167
highlight ALEWarning cterm=underline ctermfg=214
highlight ALEInfo cterm=underline ctermfg=214

let g:airline#extensions#ale#enabled = 1
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
