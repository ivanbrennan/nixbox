local arena = require('arena')

arena.setup({
  ignore_current = true,
  max_items = 24,
  devicons = false,
  algorithm = {
    recency_factor = 0.5,
    frequency_factor = 1,
  },
  window = {
    opts = {
      number = false,
    },
  },
  keybinds = {
    ["q"] = {
      function(win)
        win:close()
      end,
      { nowait = true },
    },
  },
})
