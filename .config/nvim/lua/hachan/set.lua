vim.o.guicursor = ""
vim.o.list = true
vim.o.listchars = "trail:¬"

-- Set highlight on search
vim.o.hlsearch = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Make line numbers default
vim.o.number = true
vim.o.relativenumber = true

-- Enable mouse mode
vim.o.mouse = ''

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"

-- Decrease update time
vim.o.updatetime = 50
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

------ Console UI & Text display ------

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'
vim.o.colorcolumn = '81'

vim.cmd('set wildmenu')
vim.o.wildmode = 'list:longest'
vim.o.wildignore = '*.swp,*.bak,*.pyc,*.class,*.un~'
vim.cmd('set wildignorecase')

vim.o.scrolloff = 5 -- keep at least 5 lines around the cursor

------ Indents and tabs ------

vim.cmd('set autoindent smartindent')   -- set the cursor at same indent as line above

vim.o.tabstop = 2                     -- number of visual spaces per TAB
vim.o.softtabstop = 2                 -- set virtual tab stop (compat for 8-wide tabs) | number of spaces in tab when editing
vim.o.shiftwidth = 2                  -- spaces for each step of (auto)indent
vim.o.expandtab = true                -- expand <TAB>s with spaces
vim.o.shiftround = true               -- always round indents to multiple of shiftwidth
vim.o.copyindent = true               -- use existing indents for new indents
vim.o.preserveindent = true           -- save as much indent structure as possible
