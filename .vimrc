if version >= 700

"------ Meta ------"

" clear all autocommands! (this comment must be on its own line)
autocmd!

" Enable 256bit color for console vim. Default is 8 bits.. most themes require a lot more.. 16-256.

" pathogen config
" call pathogen#infect()
" call pathogen#helptags()

" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins.
Plug 'morhetz/gruvbox'
"Plug 'nanotech/jellybeans.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'pangloss/vim-javascript'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()

set t_Co=256

set background=dark
colors gruvbox

"------Remapping leader key to space------"
let mapleader=" "

" adjust color
syntax on

" Windows wsl copy clipboard
let s:clip = '/mnt/c/Windows/System32/clip.exe'  " change this path according to your mount point
if executable(s:clip)
    augroup WSLYank
        autocmd!
        autocmd TextYankPost * if v:event.operator ==# 'y' | call system('cat |' . s:clip, @0) | endif
    augroup END
endif

" Common configs
set clipboard=unnamedplus
set nocompatible                " break away from old vi compatibility
set fileformats=unix,dos,mac    " support all three newline formats
set fileencodings=ucs-bom,utf-8,sjis,default
set viminfo=                    " don't use or save viminfo files
set encoding=utf-8
set hidden                      " hides buffers instead of closing them (allow open a new file while on unwritten changed file)
set history=1000                " remember more commands and search history
set undolevels=1000             " use many muchos levels of undo
set pastetoggle=<F2>            " when in insert mode, press <F2> to go to paste mode, where you can paste mass data that won't be autoindented
set switchbuf=useopen           " reveal already opened files from the quickfix window instead of opening new buffers
set undofile                    " create .un~ file when edit a file.
" set autochdir                   " change directories upon opening a file

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
set wildignorecase
"set cursorline cursorcolumn
set colorcolumn=81
set cul cuc                     " highlight current line
" hi CursorLine cterm=NONE ctermbg=8 ctermfg=NONE guibg=darkred guifg=white
" hi CursorColumn cterm=NONE ctermbg=8 ctermfg=NONE guibg=darkred guifg=white

"set none background
hi Normal ctermbg=none
hi NonText ctermbg=none

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
nnoremap <leader>n :noh<CR>
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
setglobal complete-=i

"------ Indents and tabs ------"

set autoindent smartindent      " set the cursor at same indent as line above
set smartindent                 " try to be smart about indenting (C-style)
set expandtab                   " expand <Tab>s with spaces; death to tabs!
set shiftwidth=2                " spaces for each step of (auto)indent
set softtabstop=2               " set virtual tab stop (compat for 8-wide tabs) | number of spaces in tab when editing
set tabstop=2                   " for proper display of files with tabs | number of visual spaces per TAB
set shiftround                  " always round indents to multiple of shiftwidth
set copyindent                  " use existing indents for new indents
set preserveindent              " save as much indent structure as possible
filetype on
filetype plugin indent on       " load filetype plugins and indent settings

"folding"
setlocal foldmethod=indent
setlocal foldignore=
hi Folded ctermbg=8

"------Miscellaneous------"
set tags=./tags;

"------Explorer Mode------"
let g:netrw_liststyle=3 "set list style similar to NerdTree

nnoremap <leader>e :E %:h<cr>

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

" Scroll other window
nmap <leader>d <C-W>W<C-D><C-W>W
nmap <leader>u <C-W>W<C-U><C-W>W

" Move between tabs
nmap <tab> gt
nmap <S-tab> gT

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Resizing Windows
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Vertically resizing
map <leader>k <C-w>+
map <leader>j <C-w>-
map <leader>h <C-w><
map <leader>l <C-w>>

"Auto resizing windows
map <leader>= <C-w>=

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Quickfix list
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"jump to next and previous entry
nnoremap <leader>p :cprev<CR>
nnoremap <leader>n :cnext<CR>

"jump to next and previous file
nnoremap <leader>fp :cpfile<CR>
nnoremap <leader>fn :cnfile<CR>

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => PLUGIN SECTION
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
"------ Neocomplcache ------"
let g:neocomplcache_enable_at_startup = 1
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_force_omni_patterns')
  let g:neocomplcache_force_omni_patterns = {}
endif
let g:neocomplcache_force_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_force_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_force_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplcache_force_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

"------ Git Gutter ------"

let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_highlight_lines = 0

highlight SignColumn ctermbg=0
"highlight SignColumn guibg=0
highlight GitGutterAddLine ctermbg=8

"------ Gundo ------"
nnoremap <F5> :GundoToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"------ Filetypes ------"
runtime macros/matchit.vim

" au BufNewFile postmortem-*.md 0r ~/postmortem-templates/templates/postmortem-template-srebook.md

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

au BufNewFile,BufRead *.json.jbuilder set ft=ruby

autocmd BufNewFile,BufRead *.slim setlocal filetype=slim

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

" Scala
autocmd FileType scala setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" JavaScript
" autocmd BufRead,BufNewFile *.json setfiletype javascript
autocmd FileType javascript setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
let javascript_enable_domhtmlcss=1

" spell check per filetype
autocmd FileType markdown setlocal spell
autocmd BufRead,BufNewFile *.md setlocal spell

autocmd FileType netrw setl bufhidden=delete

augroup HighlightTrailingSpaces
  autocmd!
  autocmd VimEnter,WinEnter,ColorScheme * highlight TrailingSpaces term=underline guibg=Red ctermbg=Red
  autocmd VimEnter,WinEnter * match TrailingSpaces /\s\+$/
augroup END

autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

"------ END VIM-500 ------"

endif " version >= 500
