.DEFAULT_GOAL := compiler.exe

clean:
	rm -f compiler.exe run.exe run.asm run.obj
	rm -rf obj

run.exe: run.obj
	gcc -o run.exe run.obj

run.obj: run.asm
	nasm -fwin32 run.asm

run.asm: run.c compiler.exe
	./compiler.exe run.c run.asm

compiler.exe: src/*.hs
	ghc -outputdir obj -o compiler.exe -isrc src/Main.hs