rgbasm -o hello.o hello.asm &&
rgblink -o hello.gb hello.o &&
rgbfix -v -p 0 hello.gb
