require'nvim-treesitter.configs'.setup {
    ensure_installed = false,
    highlight = {
        enable = true,
    },
    indent = {
        enable = true,
    }
}

-- Add treesitter to completion sources
local completion_chain_complete_list = vim.api.nvim_get_var("completion_chain_complete_list")
table.insert(completion_chain_complete_list.default.default[1].complete_items, "ts")
vim.api.nvim_set_var("completion_chain_complete_list", completion_chain_complete_list)
