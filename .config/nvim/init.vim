" plugins
call plug#begin('~/.local/share/nvim/plugged')
" General
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'scrooloose/nerdtree'
Plug 'qpkorr/vim-bufkill'
" Airline
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
" Syntax
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'neovimhaskell/haskell-vim'
" Themes
Plug 'altercation/vim-colors-solarized'
Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'morhetz/gruvbox'
Plug 'dikiaap/minimalist'
Plug 'liuchengxu/space-vim-dark'
Plug 'tomasiser/vim-code-dark'
Plug 'colepeters/spacemacs-theme.vim'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/seoul256.vim'
Plug 'NLKNguyen/papercolor-theme'
call plug#end()

" general settings
set number relativenumber
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
set showcmd
set wildmenu
set incsearch
set hlsearch
set mouse=a
set clipboard=unnamedplus
set autoread
set hidden
" colors, font, syntax

filetype plugin indent on
syntax on 
set t_Co=256
set encoding=utf-8
set background=dark
colorscheme minimalist

" bindings
let mapleader = " "
nmap <F2> :mksession! ~/.vim_session<CR> 
nmap <F3> :source ~/.vim_session<CR> 
autocmd FileType cpp nnoremap <silent> <F5> :<C-U>!g++ -Wall -g -O2 % -o ~/Code/a.out && timeout 4s ~/Code/./a.out < ~/Code/input.txt > ~/Code/output.txt<CR><CR>
nmap <C-A> ggvG$
vmap <C-C> "+y
nmap <C-C> "+yy
map <leader>p "+p
map <C-S> :w<CR>
" clear search
map <C-l> :noh<CR>
" buffers
nmap <leader>n :enew<cr>
nmap <leader>l :bn<CR>
nmap <leader>h :bp<CR>
nmap <leader>bl :ls<CR>
nmap <leader>q :bp <BAR> bd #<CR>
nmap <leader>d :bd<CR>

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='minimalist'
" utilsnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
