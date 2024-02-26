return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require("fzf-lua").setup({"fzf-vim"})

    vim.keymap.set('n', '<leader>f?', require('fzf-lua').oldfiles, { desc = '[?] Find recently opened files' })
    vim.keymap.set('n', '<leader>ff', require('fzf-lua').files, { desc = '[f] Find recently opened files' })
    vim.keymap.set('n', '<leader>fb', require('fzf-lua').buffers, { desc = '[b] Find existing buffers' })
    vim.keymap.set('n', '<leader>f/', require('fzf-lua').grep_curbuf, { desc = '[/] Fuzzily search in current buffer' })
  end
}
