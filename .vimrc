"pathogen
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

"Settings to set slime plugin target to tmux
let g:slime_target = "tmux"
let g:slime_paste_file = "$HOME/.slime_paste"

"filetype off
syntax on
"filetype plugin on

"defaults for indentation
set tabstop=4
set autoindent
set expandtab

" No textwidth for html files and 2-spaced tabs
autocmd FileType html setlocal expandtab shiftwidth=2 softtabstop=2 tabstop=2 textwidth=0 foldmethod=indent

"python indentation with spaces
autocmd FileType python setlocal expandtab shiftwidth=4 softtabstop=4 foldmethod=indent 

"Show line numbers
set number

"Background settings to make colors brighter
set background=dark

"Incremental search
set incsearch

"Highlight search results
"set hlsearch

"setting the colorscheme
"colorscheme Mustang
set t_Co=256

"Key binding to remove highlights 
nmap <silent> <C-N> :silent noh<CR>

"auto word wrapping
set textwidth=100

"For temporary files and backups. They will be saved in /tmp/
set backupdir=/tmp
set directory=/tmp



""""""""" Key bindings"""""""""""

" zencoding abbr expansion 
"let g:user_zen_expandabbr_key='<C-k>'
"imap <C-L> <C-k><cr><Esc>O<Tab>

