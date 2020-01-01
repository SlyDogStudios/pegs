ca65 pegs.asm
ld65 -C pegs.cfg -o pegs.prg pegs.o
copy /b pegs.hdr+pegs.prg+g\pegs.chr+g\pegs.chr Pegs.nes
pause
