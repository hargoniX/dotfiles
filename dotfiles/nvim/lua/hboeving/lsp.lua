-- Add language server to the completion sources
local completion_chain_complete_list = vim.api.nvim_get_var("completion_chain_complete_list")
table.insert(completion_chain_complete_list.default.default[1].complete_items, "lsp")
vim.api.nvim_set_var("completion_chain_complete_list", completion_chain_complete_list)

-- Setup all the servers
if vim.api.nvim_eval("executable('rust-analyzer')") then
    require'nvim_lsp'.rust_analyzer.setup{}
end

if vim.api.nvim_eval("executable('solargraph')") then
    require'nvim_lsp'.solargraph.setup{}
end

if vim.api.nvim_eval("executable('texlab')") then
    require'nvim_lsp'.texlab.setup{
      settings = {
        latex = {
          build = {
            onSave = true;
          };
          forwardSearch = {
            executable = "zathura";
            args = {"--synctex-forward", "%l:1:%f", "%p"};
          }
        }
      };
      commands = {
        ZathuraShow = {
            function()
              vim.lsp.buf_request(0, "textDocument/forwardSearch", vim.lsp.util.make_position_params(),
                function(err, _, _, _)
                  if err then error(tostring(err)) end
                end
              )
            end;
            description = "Show the current position in zathura";
        }
      }
    }
    vim.api.nvim_set_keymap("n", "gz", "<cmd>ZathuraShow<CR>", { noremap = true, silent = true })
end

if vim.api.nvim_eval("executable('pyls')") then
    require'nvim_lsp'.pyls.setup{}
end

if vim.api.nvim_eval("isdirectory($HOME. '/.cache/nvim/nvim_lsp/jdtls')") then
    require'nvim_lsp'.jdtls.setup{}
end

-- g(o) d(efinition)
vim.api.nvim_set_keymap("n", "gd" , "<cmd>lua vim.lsp.buf.definition()<CR>", { noremap = true, silent = true })
-- g(o) W(orkspace)...probably not the most inutitive mnmemonic for this
vim.api.nvim_set_keymap("n", "gW" , "<cmd>lua vim.lsp.buf.document_symbol()<CR>", { noremap = true, silent = true })
-- K for hover information
vim.api.nvim_set_keymap("n", "K" , "<cmd>lua vim.lsp.buf.hover()<CR>", { noremap = true, silent = true })

-- Rebind f(zf) c(tags)
vim.api.nvim_set_keymap("n", "<leader>fc", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", { noremap = true })
-- c(hange) n(ame)
vim.api.nvim_set_keymap("n", "<leader>cn", "<cmd>lua vim.lsp.buf.rename()<CR>", { noremap = true })
-- c(ode) a(ction)
vim.api.nvim_set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", { noremap = true })
-- i(ncoming) c(alls)
vim.api.nvim_set_keymap("n", "<leader>ic", "<cmd>lua vim.lsp.buf.incoming_calls()<CR>", { noremap = true })
-- s(how) r(eferences), shows references
vim.api.nvim_set_keymap("n", "<leader>sr", "<cmd>lua vim.lsp.buf.references()<CR>", { noremap = true})
-- s(how) d(iagnostics)
vim.api.nvim_set_keymap("n", "<leader>sd", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", { noremap = true})

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        -- Underline my errors
        underline = true,
        -- Don't do diagnostics while I am typing.
        update_in_insert = false
    }
)
