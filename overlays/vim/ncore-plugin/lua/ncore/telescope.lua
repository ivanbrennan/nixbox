-- https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/mappings.lua

local telescope = require('telescope')
local actions = require('telescope.actions')
local layout = require('telescope.actions.layout')
local fb_actions = require('telescope').extensions.file_browser.actions
local o = vim.o

telescope.setup({
  defaults = {
    layout_strategy = 'center',
    layout_config = {
      center = {
        anchor = 'S',
        height = 0.45,
        width = function(_, max_columns, _)
          return max_columns
        end,
      },
    },
    sorting_strategy = 'ascending',
    prompt_prefix = '  ',
    selection_caret = '  ',
    multi_icon = '⋅',
    border = true,
    borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    hl_result_eol = true,
    results_title = false,
    history = { limit = 1000 },
    preview = { hide_on_startup = false },
    mappings = {
      i = {
        ["<C-h>"] = actions.which_key,

        ["<C-j>"] = actions.preview_scrolling_down,
        ["<C-k>"] = actions.preview_scrolling_up,
        ["<C-d>"] = false,
        ["<C-u>"] = false,

        ["<M-n>"] = actions.cycle_history_next,
        ["<M-p>"] = actions.cycle_history_prev,

        ["<C-y>"] = layout.toggle_preview,

        ["<Esc>"] = actions.close,
        -- TODO: What's the use-case for normal mode?
        ["<C-c>"] = false,
      },
      n = {
        ["<C-y>"] = layout.toggle_preview,
      },
    },
  },
  pickers = {
    grep_string = {
      mappings = {
        i = {
          ["<C-r>"] = actions.to_fuzzy_refine,
        },
      },
    },
    live_grep = {
      mappings = {
        i = {
          ["<C-r>"] = actions.to_fuzzy_refine,
        },
      },
    },
  },
  extensions = {
    fzf = {
      fuzzy = false,                  -- exact matching, space separated AND, | OR
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true,    -- override the file sorter
      case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
                                      -- the default case_mode is "smart_case"
    },
    file_browser = {
      theme = 'ivy',
      hijack_netrw = false, -- dirvish still takes precedence
      dir_icon = '',
      mappings = {
        ['i'] = {
          ['<A-d>'] = false,
          ['<A-r>'] = false,
          ['<C-e>'] = false,
          ['<C-f>'] = false,
          ['<C-t>'] = false,
          ['<C-w>'] = { '<C-S-w>', type = 'command' },
          ['<C-S-n>'] = fb_actions.create,
          ['<C-S-d>'] = fb_actions.remove,
          ['<C-S-r>'] = fb_actions.rename,
          ['<A-g>'] = fb_actions.toggle_browser,
          ['<C-.>'] = fb_actions.toggle_hidden,
          ['<C-S-a>'] = fb_actions.toggle_all,
        },
      },
    },
    undo = {
      use_delta = true,
      side_by_side = false, -- TODO: make undo() accept parameters
    },
  },
})

require('telescope').load_extension('fzf')
require('telescope').load_extension('file_browser')
require('telescope').load_extension('undo')
