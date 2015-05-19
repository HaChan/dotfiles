if version >= 700

"------ Meta ------"

" clear all autocommands! (this comment must be on its own line)
autocmd!

" Enable 256bit color for console vim. Default is 8 bits.. most themes require a lot more.. 16-256.
set t_Co=256
set background=light
"colors desert

" pathogen config
call pathogen#infect()
call pathogen#helptags()

set nocompatible                " break away from old vi compatibility
set fileformats=unix,dos,mac    " support all three newline formats
set viminfo=                    " don't use or save viminfo files
set encoding=utf-8
set hidden                      " hides buffers instead of closing them (allow open a new file while on unwritten changed file)
set history=1000                " remember more commands and search history
set undolevels=1000             " use many muchos levels of undo
set pastetoggle=<F2>            " when in insert mode, press <F2> to go to paste mode, where you can paste mass data that won't be autoindented
set switchbuf=useopen           " reveal already opened files from the quickfix window instead of opening new buffers
set undofile                    " create .un~ file when edit a file.

"------ Console UI & Text display ------"
set cmdheight=2                 " explicitly set the height of the command line
set showcmd                     " Show (partial) command in status line.
set number                      " yay line numbers
set ruler                       " show current position at bottom
set novisualbell
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
set wildignore=*.swp,*.bak,*.pyc,*.class
set cul cuc                     " highlight current line
"set cursorline cursorcolumn
set colorcolumn=81
hi CursorLine term=none cterm=none ctermbg=0
hi CursorColumn term=none cterm=none ctermbg=0
" adjust color
syntax on
" set mispelled text highlight to underline
hi clear SpellBad
hi SpellBad cterm=underline
" set region to US English
set spelllang=en_us

"------set what display in the status
set statusline=%f "tail of the filename
set statusline+=%h "help file flag
set statusline+=%y "filetype
set statusline+=%r "read only flag
set statusline+=%m "modified flag
set statusline+=%c, "cursor column
set statusline+=%l/%L "cursor line/total lines
set statusline+=\ %P "percent through file
set laststatus=2

"------ Text editing and searching behavior ------"

set hlsearch                  " turn off highlighting for searched expressions
" This unsets the last search pattern register by hitting return
nnoremap <CR> :noh<CR>
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

"------Miscellaneous------"
set tags=./tags;

""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>

" Reselect text that was just pasted with ,v
nnoremap <leader>v V`]

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Opens a new tab with the current buffer's path
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Move between buffers
nmap <C-p> <Esc>:bp<CR>
nmap <C-n> <Esc>:bn<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.* :call DeleteTrailingWS()

"close parenthesis
inoremap (      ()<Left>
inoremap (<CR>  (<CR>)<Esc>O
inoremap ((     (
inoremap ()     ()

" Complete whole filenames/lines with a quicker shortcut key in insert mode
inoremap <C-f> <C-x><C-f>
inoremap <C-l> <C-x><C-l>

" Toggle spell checking on and off with `leader s`
nmap <silent> <leader>s :set spell!<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("Ack \"" . l:pattern . "\" " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction


"------ Filetypes ------"
runtime macros/matchit.vim

" markdown
au BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

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

" spell check per filetype
autocmd FileType markdown setlocal spell
autocmd BufRead,BufNewFile *.md setlocal spell

augroup HighlightTrailingSpaces
  autocmd!
  autocmd VimEnter,WinEnter,ColorScheme * highlight TrailingSpaces term=underline guibg=Red ctermbg=Red
  autocmd VimEnter,WinEnter * match TrailingSpaces /\s\+$/
augroup END

"------ END VIM-500 ------"

endif " version >= 500
