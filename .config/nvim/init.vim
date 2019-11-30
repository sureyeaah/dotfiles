" plugins
call plug#begin('~/.local/share/nvim/plugged')
" General
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'scrooloose/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'qpkorr/vim-bufkill'
Plug 'easymotion/vim-easymotion'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'terryma/vim-smooth-scroll'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
" Syntax
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'neovimhaskell/haskell-vim'
" Themes
Plug 'altercation/vim-colors-solarized'
Plug 'morhetz/gruvbox'
call plug#end()

" general settings
set number relativenumber
set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
set showcmd
set wildmenu
set incsearch
set hlsearch
set mouse=a
set clipboard=unnamedplus
set autoread
set hidden
"set signcolumn=yes
" colors, font, syntax

filetype plugin indent on
syntax on 
set t_Co=256
set encoding=utf-8
set background=dark
colorscheme gruvbox
autocmd BufRead *.sql set filetype=mysql      
set cmdheight=2
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" bindings
let mapleader = " "
nmap <F2> :mksession! ~/.vim_session<CR> 
nmap <F3> :source ~/.vim_session<CR> 
autocmd FileType cpp nnoremap <silent> <F5> :<C-U>!g++ -Wall -g -O2 % -o ~/Code/a.out && timeout 4s ~/Code/./a.out < ~/Code/input.txt > ~/Code/output.txt<CR>
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
nmap <leader>d :BD<CR>

" Move between splits
nnoremap <leader>mh <C-w>h
nnoremap <leader>mj <C-w>j
nnoremap <leader>mk <C-w>k
nnoremap <leader>ml <C-w>l

" Close upper buffer
nmap <leader>u <C-w>k:q<CR>

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" smooth scroll
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 8, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 8, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 8, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 8, 4)<CR>

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme='gruvbox'
let g:airline_powerline_fonts = 1

" coc
let g:coc_global_extensions = ['coc-pairs', 'coc-snippets',]
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)
" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" NERDtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
"Toggle NERDTree with Ctrl-N
map <C-n> :NERDTreeToggle<CR>
"Show hidden files in NERDTree
let NERDTreeShowHidden=1

" utilsnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" haskell
autocmd Filetype haskell setlocal tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab
let g:cabal_indent_section = 2
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
"let g:haskell_classic_highlighting = 1
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_indent_if = 3
let g:haskell_indent_case = 2
let g:haskell_indent_let = 4
let g:haskell_indent_where = 6
let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_do = 3
let g:haskell_indent_in = 1
let g:haskell_indent_guard = 2
let g:haskell_indent_case_alternative = 1
