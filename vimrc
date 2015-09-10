" Call pathogen if it's installed
call pathogen#infect()

""""
" General Settings
""""

set history=500

" File based settings and indentation
filetype plugin on
filetype indent on

" Auto reload files
set autoread

" define a leader symbol
let mapleader = ","
let g:mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

" :W sudo saves the file
" Useful for avoiding permission-denied errors
command W w !sudo tee % > /dev/null

""""
" VIM Interface Settings
""""

" set j/k to move by 7 lines
set so=7

" Enable the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*

" Show the ruler
set ruler

" Show line numbers
set number

" Command bar height should be 2 rows
set cmdheight=2

" Hide buffers when they're abandoned
set hid

" Configure backspace
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ingore case while searching
set ignorecase

" Be smart about case while searching
set smartcase

" Highlight results
set hlsearch

" Search more like modern browsers
set incsearch

" Avoid redrawing during macros
set lazyredraw

" For regular expressions
set magic

" Show matching brackets
set showmatch
set mat=2

" Disable annoying sounds
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Extra column on the left
set foldcolumn=1

""""
" Colors and Fonts
""""

" Enable syntax highlighting
syntax enable

" Set the vim editor color scheme
:color desert

" Optimize colors for a dark terminal
set background=dark

" Default to a utf8 encoding
set encoding=utf8

" Unix will be the default file type
set ffs=unix,dos,mac

""""
" Text, Tab, Indent
""""

" Expand Tabs
set expandtab

" Smart Tabs
set smarttab

" Tabs are 4 spaces
set shiftwidth=4
set tabstop=4
set softtabstop=4

" 500 character max per line
set lbr
set tw=500

" Auto Indent, Smart Indent and Line Wrapping
set ai
set si
set wrap

""""
" Movement, Tabs, Buffers, Windows
""""

" Prevent movement to the first char on the line
set nostartofline

" Quickly timeout on keycodes, but never on mappings
set notimeout ttimeout ttimeoutlen=200

" Long lines are break lines
map j gj
map k gk

" Better movement between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Remap to arrows to navigate wrapped lines
imap <silent> <Down> <C-o>gj
imap <silent> <Up> <C-o>gk
nmap <silent> <Down> gj
nmap <silent> <Up> gk

""""
" Status Line
""""

" Show the status line
set laststatus=2

" Format for the status line
set statusline=\ %{HasPaste()}
set statusline+=%F "File
set statusline+=%m%r%h "File mode/status
set statusline+=\ %y "File type
set statusline+=\ %w 
set statusline+=\ \ CWD:\ %r%{getcwd()}%h
set statusline+==%= " left/right seperator
set statusline+=Line:\ %l.%c/%L "line and column count
set statusline+=\ %P "percentage of document

" Set the statusline color based on the current mode
au InsertEnter * call InsertStatusLineColor(v:insertmode)
au InsertLeave * hi Statusline ctermfg=black ctermbg=white guifg=black guibg=white

" Grey status line on first entrance
hi Statusline ctermfg=black ctermbg=white guifg=black guibg=white

""""
" Spell Checking
""""

" Pressing ,ss will toggle spell checking
map <leader>ss :setlocal spell!<cr>

" Leader shortcuts
map <leader>sn ]s " Next Spelling Mistake
map <leader>sp [s " Previous Spelling Mistake
map <leader>sa zg " Add Spelling
map <leader>s? z= " Search Spelling

""""
" Misc
""""

" Toggle paste mode
map <leader>pp :setlocal paste!<cr

""""
" Helper Functions
""""

function! InsertStatusLineColor(mode)
    " Insert Mode
    if a:mode == 'i'
        hi Statusline ctermfg=black ctermbg=green guifg=black guibg=green
    " Replace Mode
    elseif a:mode == 'r'
        hi Statusline ctermfg=black ctermbg=darkcyan guifg=black guibg=darkcyan
    " All Others (Visual,Select,Command-line,EX)
    else
        hi Statusline ctermfg=black ctermbg=darkred guifg=black guibg=darkred
    endif
endfunction

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'Paste Mode '
    endif
    return ''
endfunction
