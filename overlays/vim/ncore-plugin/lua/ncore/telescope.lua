-- https://github.com/nvim-telescope/telescope.nvim/blob/master/lua/telescope/mappings.lua

local telescope = require('telescope')
local actions = require('telescope.actions')
local layout = require('telescope.actions.layout')
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
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
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
    live_grep = {
      mappings = {
        i = {
          ["<C-r>"] = actions.to_fuzzy_refine,
        },
      },
    },
  },
})
