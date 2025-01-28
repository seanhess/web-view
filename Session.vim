let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Projects/Work/GasWork/RelatedWork/web-view
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +500 src/Web/View/Types.hs
badd +277 src/Web/View/Style.hs
badd +68 ~/.local/share/nvim/parrot/chats/2025-01-23.22-56-48.041.md
badd +66 web-view.cabal
badd +251 ~/.local/share/nvim/parrot/chats/2025-01-25.06-42-05.937.md
badd +1 src/Web/View/Types
argglobal
%argdel
$argadd src/Web/View/Types
edit src/Web/View/Style.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe '1resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 1resize ' . ((&columns * 91 + 91) / 182)
exe '2resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 2resize ' . ((&columns * 91 + 91) / 182)
exe '3resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 3resize ' . ((&columns * 90 + 91) / 182)
exe '4resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 4resize ' . ((&columns * 90 + 91) / 182)
argglobal
balt src/Web/View/Types.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 277 - ((18 * winheight(0) + 19) / 38)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 277
normal! 0
lcd ~/Projects/Work/GasWork/RelatedWork/web-view
wincmd w
argglobal
if bufexists(fnamemodify("~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs", ":p")) | buffer ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs | else | edit ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs | endif
if &buftype ==# 'terminal'
  silent file ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs
endif
balt ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 499 - ((30 * winheight(0) + 19) / 38)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 499
normal! 02|
lcd ~/Projects/Work/GasWork/RelatedWork/web-view
wincmd w
argglobal
if bufexists(fnamemodify("~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs", ":p")) | buffer ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs | else | edit ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs | endif
if &buftype ==# 'terminal'
  silent file ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs
endif
balt ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
silent! normal! zE
31,63fold
87,112fold
150,168fold
276,279fold
392,413fold
442,470fold
510,519fold
let &fdl = &fdl
let s:l = 125 - ((24 * winheight(0) + 19) / 38)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 125
normal! 0
lcd ~/Projects/Work/GasWork/RelatedWork/web-view
wincmd w
argglobal
if bufexists(fnamemodify("~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs", ":p")) | buffer ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs | else | edit ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs | endif
if &buftype ==# 'terminal'
  silent file ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Types.hs
endif
balt ~/Projects/Work/GasWork/RelatedWork/web-view/src/Web/View/Style.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
silent! normal! zE
31,63fold
87,112fold
150,168fold
276,279fold
392,413fold
442,470fold
510,519fold
let &fdl = &fdl
let s:l = 268 - ((19 * winheight(0) + 19) / 38)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 268
normal! 0
lcd ~/Projects/Work/GasWork/RelatedWork/web-view
wincmd w
exe '1resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 1resize ' . ((&columns * 91 + 91) / 182)
exe '2resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 2resize ' . ((&columns * 91 + 91) / 182)
exe '3resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 3resize ' . ((&columns * 90 + 91) / 182)
exe '4resize ' . ((&lines * 38 + 39) / 79)
exe 'vert 4resize ' . ((&columns * 90 + 91) / 182)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
let g:this_session = v:this_session
let g:this_obsession = v:this_session
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
