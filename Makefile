
main: emu6809
	ghc Comp6.hs

emu6809: emu6809.o mpu6809.o ROM.o
	$(CC) -o $@ $^

emu6809.o: emu6809.c mpu6809.h ROM.h
mpu6809.o: mpu6809.c mpu6809.h
ROM.o: ROM.c ROM.h


clean:
	rm -rf *.o *.hi Comp6 emu6809