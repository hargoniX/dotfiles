local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

vim.g.maplocalleader = " "

require("lazy").setup({
  {
    "ellisonleao/gruvbox.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.o.background = "light"
      vim.cmd([[colorscheme gruvbox]])
    end,
  },
  {
    "nmac427/guess-indent.nvim",
    lazy = false,
    priority = 999,
    config = function()
      require("guess-indent").setup {}
    end,
  },
  {
    "linrongbin16/lsp-progress.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("lsp-progress").setup()
    end
  },
  {
    -- integrate with lualine
    "nvim-lualine/lualine.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "linrongbin16/lsp-progress.nvim",
    },
    config = function()
      require("lualine").setup({
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "filename" },
          lualine_c = {
              -- invoke `progress` here.
              require("lsp-progress").progress(),
          }
        }
      })
      -- listen lsp-progress event and refresh lualine
      vim.api.nvim_create_augroup("lualine_augroup", { clear = true })
      vim.api.nvim_create_autocmd("User LspProgressStatusUpdated", {
        group = "lualine_augroup",
        callback = require("lualine").refresh,
      })
    end
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {},
  },
  {
    "nvim-telescope/telescope.nvim", tag = "0.1.3",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>ff", "<cmd>lua require('telescope.builtin').find_files()<cr>" },
      { "<leader>fg", "<cmd>lua require('telescope.builtin').live_grep()<cr>" },
      { "<leader>fb", "<cmd>lua require('telescope.builtin').buffers()<cr>" },
    },
  },
  {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "2.*",
    event = "InsertEnter",
    config = function()
      require("luasnip.loaders.from_snipmate").lazy_load({ paths = { "~/.config/nvim/lua/hboeving/snippets/" } })
    end
  },
  {
    "Julian/lean.nvim",
    ft = "lean",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },

    -- see details below for full configuration options
    opts = {
      lsp = {
        on_attach = on_attach,
      },
      mappings = true,
    }
  },
  {
    "nvim-orgmode/orgmode",
    dependencies = {
      { "nvim-treesitter/nvim-treesitter", lazy = true },
    },
    ft = "org",
    keys = {"<leader>oa", "<leader>oc"},
    config = function()
      -- Setup treesitter
      require("nvim-treesitter.configs").setup({
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = { "org" },
        },
        ensure_installed = { "org" },
      })

      -- Load treesitter grammar for org
      require("orgmode").setup_ts_grammar()

      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/org/*",
        org_default_notes_file = "~/org/notes.org",
        org_todo_keywords = {"TODO(t)", "PROGRESS(p)", "|", "DONE(d)"},
        org_log_done = nil,
        org_highlight_latex_and_related = "entities",
      })

      vim.cmd([[hi OrgAgendaScheduled guifg=Grey]])
      vim.cmd([[hi OrgAgendaDeadline guifg=Grey]])
    end,
  },
  {
    "neovim/nvim-lspconfig",
    ft = { "python", "rust", "lean", "haskell" },
    dependencies = {
      "hrsh7th/nvim-cmp",
    },
    config = function()
      local lspconfig = require"lspconfig"
      -- The nvim-cmp almost supports LSP"s capabilities so You should advertise it to LSP servers..
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- The following example advertise capabilities to `clangd`.
      lspconfig.rust_analyzer.setup { capabilities = capabilities }
      lspconfig.pyright.setup { capabilities = capabilities }
      lspconfig.hls.setup { capabilities = capabilities }

      -- Use LspAttach autocommand to only map the following keys
      -- after the language server attaches to the current buffer
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(ev)
          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
          vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
          vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
          vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
          vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
          vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, opts)
          vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, opts)
          vim.keymap.set("n", "<space>wl", function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts)
          vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, opts)
          vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, opts)
          vim.keymap.set({ "n", "v" }, "<space>ca", vim.lsp.buf.code_action, opts)
          vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
          vim.keymap.set("n", "<space>f", function()
            vim.lsp.buf.format { async = true }
          end, opts)
        end,
      })
    end
  },
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "saadparwaiz1/cmp_luasnip",
      "FelipeLema/cmp-async-path",
      "ray-x/cmp-treesitter",
      "petertriho/cmp-git",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "saadparwaiz1/cmp_luasnip"
    },
    config = function()
      local cmp = require"cmp"
      local luasnip = require("luasnip")

      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      cmp.setup {
        snippet = {
          expand = function(args)
            require"luasnip".lsp_expand(args.body)
          end
        },

        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "treesitter" },
          { name = "luasnip" },
          { name = "async_path" },
        }, {
          { name = "buffer" }
        }),

        mapping = cmp.mapping.preset.insert({
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable() 
            -- they way you will only jump inside the snippet region
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
      }

      cmp.setup.filetype("gitcommit", {
        sources = cmp.config.sources({
          { name = "git" },
        }, {
          { name = "buffer" },
        })
      })

    end,
  },
})

