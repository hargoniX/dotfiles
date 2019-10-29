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
