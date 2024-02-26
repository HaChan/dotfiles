vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Move highlighted block
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- Paste without modified the buffer (using the void buffer _)
vim.keymap.set("x", "<leader>p", [["_dP]])

-- Delete without saving to the register
vim.keymap.set({"n", "v"}, "<leader>d", [["_d]])

-- Move between windows
vim.keymap.set("n", "<C-k>", "<C-W>k", {noremap = true})
vim.keymap.set("n", "<C-j>", "<C-W>j", {noremap = true})
vim.keymap.set("n", "<C-h>", "<C-W>h", {noremap = true})
vim.keymap.set("n", "<C-l>", "<C-W>l", {noremap = true})

-- Move tabs
vim.keymap.set("n", "<tab>", "gt", {noremap = true})
vim.keymap.set("n", "<S-tab>", "gT", {noremap = true})
