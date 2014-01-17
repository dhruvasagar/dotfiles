" Use repeat operator with visual selection {{{1
vnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$
nnoremap Q :q<CR>

" Common Mappings {{{1
nnoremap <C-S> :w<CR>
nnoremap <C-X>s :wa<CR>
nnoremap <C-X><C-S> :w<CR>
nnoremap <C-X><C-C> :q<CR>

" These are mappings for the longlines mode equivalent {{{1
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

nnoremap <Up> gk
nnoremap <Down> gj
vnoremap <Up> gk
vnoremap <Down> gj
inoremap <Up> <C-o>gk
inoremap <Down> <C-o>gj

" Select all text in buffer {{{1
nnoremap <Leader>a ggVG

" Format entire document {{{1
nnoremap <Leader>= gg=G

" Clear search highlights {{{1
nnoremap <Leader>/ :nohl<CR>

" Focus on current buffer {{{1
nnoremap <Leader>o :tab split<CR>

" Bubble multiple lines - also dependent on vim-unimpaired plugin {{{1
xmap [E [egv
xmap ]E ]egv

" Dispatch {{{1
nnoremap <Leader>D :Dispatch<CR>

" Fugitive {{{1
nnoremap <silent> <Leader>ga :Gwrite<CR>
nnoremap <silent> <Leader>gd :Gdiff<CR>
nnoremap <silent> <Leader>gs :Gstatus<CR>
nnoremap <silent> <Leader>gcd :Gcd<Bar>pwd<CR>

" Tabular mapping {{{1
xnoremap <Leader>/ :Tabularize/
xnoremap <Leader>= :Tabularize/=<CR>
xnoremap <Leader>: :Tabularize/:<CR>

" NERDTree mappings {{{1
nnoremap <Leader>nT :NERDTree<CR>
nnoremap <Leader>nt :NERDTreeToggle<CR>
nnoremap <Leader>nf :NERDTreeFind<CR>

" Neo Bundle {{{1
nnoremap <Leader>nu :Unite -log -tab -wrap neobundle/update<CR>
