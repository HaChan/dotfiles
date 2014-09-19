if version >= 700

"------ Meta ------"

" clear all autocommands! (this comment must be on its own line)
autocmd!

" Enable 256bit color for console vim. Default is 8 bits.. most themes require a lot more.. 16-256.
set t_Co=256
set background=light
"colors desert

set nocompatible                " break away from old vi compatibility
set fileformats=unix,dos,mac    " support all three newline formats
set viminfo=                    " don't use or save viminfo files

" These two remove annoying blinking and noise
set novisualbell
set noerrorbells

"------ Console UI & Text display ------"
set cmdheight=1                 " explicitly set the height of the command line
set showcmd                     " Show (partial) command in status line.
set number                      " yay line numbers
set ruler                       " show current position at bottom
set noerrorbells                " don't whine
set visualbell t_vb=            " and don't make faces
set lazyredraw                  " don't redraw while in macros
set scrolloff=5                 " keep at least 5 lines around the cursor
set wrap                        " soft wrap long lines
set list                        " show invisible characters
set listchars=tab:>·,trail:·    " but only show tabs and trailing whitespace
set report=0                    " report back on all changes
set shortmess=atI               " shorten messages and don't show intro
set wildmenu                    " turn on wild menu :e <Tab>
set wildmode=list:longest       " set wildmenu to list choice
set cul cuc                     " highlight current line
"set cursorline cursorcolumn
set colorcolumn=80
"hi CursorLine term=none cterm=none ctermbg=0      " adjust color
syntax on

"------ Text editing and searching behavior ------"

set hlsearch                  " turn off highlighting for searched expressions
set incsearch                   " highlight as we search however
hi Search guibg=peru guifg=wheat
hi Search cterm=NONE ctermfg=green ctermbg=red
"set matchtime=5                 " blink matching chars for .x seconds
"set mouse=a                     " try to use a mouse in the console (wimp!)
set ignorecase                  " set case insensitivity
set smartcase                   " unless there's a capital letter
set completeopt=menu,longest,preview " more autocomplete <Ctrl>-P options
"set nostartofline               " leave my cursor position alone!
set backspace=2                 " equiv to :set backspace=indent,eol,start
set textwidth=80                " we like 80 columns
set showmatch                   " show matching brackets
set formatoptions=tcrql         " t - autowrap to textwidth
                                " c - autowrap comments to textwidth
                                " r - autoinsert comment leader with <Enter>
                                " q - allow formatting of comments with :gq
                                " l - don't format already long lines

"------ Indents and tabs ------"

set autoindent smartindent      " set the cursor at same indent as line above
set smartindent                 " try to be smart about indenting (C-style)
set expandtab                   " expand <Tab>s with spaces; death to tabs!
set shiftwidth=2                " spaces for each step of (auto)indent
set softtabstop=2               " set virtual tab stop (compat for 8-wide tabs)
set tabstop=2                   " for proper display of files with tabs
set shiftround                  " always round indents to multiple of shiftwidth
set copyindent                  " use existing indents for new indents
set preserveindent              " save as much indent structure as possible
filetype on
filetype plugin indent on       " load filetype plugins and indent settings

"------ Filetypes ------"

" Vimscript
autocmd FileType vim setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

" Shell
autocmd FileType sh setlocal expandtab shiftwidth=4 tabstop=8 softtabstop=4

" Lisp
autocmd Filetype lisp,scheme setlocal equalprg=~/.vim/bin/lispindent.lisp expandtab shiftwidth=2 tabstop=8 softtabstop=2

" Ruby
autocmd FileType ruby,rb,erb setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" Python
autocmd FileType python,py setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

" PHP
autocmd FileType php setlocal expandtab shiftwidth=4 tabstop=4 softtabstop=4

" X?HTML & XML
autocmd FileType html,xhtml,xml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

autocmd FileType html.erb setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

autocmd FileType html set omnifunc=htmlcomplete#CompleteTags

" CSS
autocmd FileType css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" JavaScript
" autocmd BufRead,BufNewFile *.json setfiletype javascript
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
let javascript_enable_domhtmlcss=1

augroup HighlightTrailingSpaces
  autocmd!
  autocmd VimEnter,WinEnter,ColorScheme * highlight TrailingSpaces term=underline guibg=Red ctermbg=Red
  autocmd VimEnter,WinEnter * match TrailingSpaces /\s\+$/
augroup END

"------ END VIM-500 ------"

endif " version >= 500
