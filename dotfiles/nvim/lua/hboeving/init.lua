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
vim.api.nvim_set_option("clipboard","unnamedplus")

local lsp_fts = { "python", "rust", "lean", "haskell", "typst", "lua", "java", "c", "cpp" }
local ts_fts = {"c", "cpp", "agda", "lua", "java", "haskell", "markdown" }

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
    ft = lsp_fts,
    config = function()
      require("lsp-progress").setup()
    end
  },
  {
    -- integrate with lualine
    "nvim-lualine/lualine.nvim",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("lualine").setup({
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "filename" },
        },
        options = { theme = "gruvbox" },
      })
    end
  },
  {
    "m4xshen/autoclose.nvim",
    event = "InsertEnter",
    config = true,
    opts = {},
  },
  {
    "ibhagwan/fzf-lua",
    keys = {
      { "<leader>ff", "<cmd>lua require('fzf-lua').files()<cr>" },
      { "<leader>fg", "<cmd>lua require('fzf-lua').live_grep()<cr>" },
      { "<leader>fb", "<cmd>lua require('fzf-lua').buffers()<cr>" },
      { "<leader>fs", "<cmd>lua require('fzf-lua').lsp_live_workspace_symbols()<cr>" },
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

    opts = {
      lsp = {
        on_attach = function()
          vim.api.nvim_create_autocmd('VimResized', { callback = require('lean.infoview').reposition })
          local mappings = {
            ['@lsp.type.variable'] = 'Identifier',
            ['@lsp.type.function'] = 'Function',
            ['@lsp.type.property'] = 'Identifier',
          }

          for from, to in pairs(mappings) do
            vim.cmd.highlight('link ' .. from .. ' ' .. to)
          end
        end,
      },
      init_options = {
        editDelay = 200,
      },
      mappings = true,
    }
  },
  {
    "nvim-treesitter/nvim-treesitter",
    ft = ts_fts,
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = ts_fts,
        auto_install = false,
        highlight = {
          enable = true,
        },
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    ft = lsp_fts,
    dependencies = {
      "saghen/blink.cmp",
    },
    config = function()
      local lspconfig = require"lspconfig"
      local capabilities = require('blink.cmp').get_lsp_capabilities()

      -- The following example advertise capabilities to `clangd`.
      lspconfig.rust_analyzer.setup { capabilities = capabilities }
      lspconfig.pyright.setup { capabilities = capabilities }
      lspconfig.hls.setup { capabilities = capabilities }
      lspconfig.clangd.setup { capabilities = capabilities }
      lspconfig.tinymist.setup{
        offset_encoding = "utf-8",
      	settings = {
          exportPdf = "onSave",
        }
      }

      lspconfig.lua_ls.setup {
        on_init = function(client)
          client.config.settings = vim.tbl_deep_extend('force', client.config.settings, {
            Lua = {
              runtime = { version = 'LuaJIT' }, -- most likely
              -- Make the server aware of Neovim runtime files
              workspace = {
                checkThirdParty = false,
                library = { vim.env.VIMRUNTIME }
              }
            }
          })
          client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
          return true
        end,
        on_attach=on_attach,
        capabilities=capabilities,
      }

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
          vim.keymap.set('n', '<space>K', function() vim.diagnostic.open_float(0, { scope = "line", header = false, focus = false }) end)
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
          vim.keymap.set("n", "gc", vim.lsp.buf.incoming_calls, opts)
          vim.keymap.set("n", "<space>f", function()
            vim.lsp.buf.format { async = true }
          end, opts)
          vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" })
          vim.lsp.inlay_hint.enable()
        end,
      })
    end
  },
  {
    "ldelossa/litee.nvim",
    ft = lsp_fts,
    config = function()
      local litee = require"litee.lib"
      litee.setup({})
    end
  },
  {
    "ldelossa/litee-calltree.nvim",
    ft = lsp_fts,
    config = function()
      local calltree = require"litee.calltree"
      calltree.setup({
        resolve_symbols = false,
        no_hls = true,
        on_open = "panel",
      })
    end
  },
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    config = function()
      local config = {
          cmd = {'jdtls'},
          root_dir = vim.fs.dirname(vim.fs.find({'gradlew', '.git', 'mvnw'}, { upward = true })[1]),
      }
      require('jdtls').start_or_attach(config)
    end
  },
  {
    'saghen/blink.cmp',
    event = "InsertEnter",
    version = 'v0.*',
    opts = {
      keymap = { preset = 'enter' },
      completion = {
        documentation = {
          auto_show = true,
          auto_show_delay_ms = 50,
        },
        accept = {
          auto_brackets = {
            enabled = true,
          }
        }
      },
      snippets = { preset = 'luasnip' },
      sources = {
        default = { 'lsp', 'path', 'snippets', 'buffer' },
        cmdline = {}
      },
    }
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },
    keys = {"<leader>gs"},
    cmd = {"Neogit"},
    config = function()
      local neogit = require("neogit")
      neogit.setup {}
      vim.keymap.set("n", "<leader>gs", neogit.open)
    end
  },
  {
    "kaarmu/typst.vim",
    ft = "typst",
    lazy=false,
  },
}, {
  dev = {
    path = "~/Desktop/nvim"
  },
})


local spell_fts = { "text", "plaintex", "tex", "typst" }

for _, spell_ft in ipairs(spell_fts) do
  vim.api.nvim_create_autocmd("FileType", {
  	pattern = spell_ft,
  	callback = function ()
      vim.opt.spelllang = "en_us,de_20"
      vim.opt.spell = true
  	end,
  })
end

local function copy_file(src, dst)
  os.execute("cp " .. src .. " " .. dst)
end

vim.api.nvim_create_user_command("OrgSync",
  function(_)
    copy_file("~/org/notes.org", "~/webdav/notes.org")
    copy_file("~/org/personal.org", "~/webdav/personal.org")
    copy_file("~/org/uni.org", "~/webdav/uni.org")
    copy_file("~/org/weekly.org", "~/webdav/weekly.org")
    copy_file("~/org/goals.org", "~/webdav/goals.org")
    copy_file("~/webdav/mobile-notes.org", "~/org/mobile-notes.org")
  end,
  { nargs = 0 }
)
