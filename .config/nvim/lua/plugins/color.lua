local catppuccin = {
  -- Theme inspired by Atom
  'catppuccin/nvim',
  name = 'catppuccin',
  priority = 1000,
  config = function()
    vim.cmd.colorscheme 'catppuccin'
  end,
}
local gruvbox = {
  'morhetz/gruvbox',
  name = 'gruvbox',
  priority = 1000,
  config = function()
    vim.cmd.colorscheme 'gruvbox'
  end,
}
return gruvbox
