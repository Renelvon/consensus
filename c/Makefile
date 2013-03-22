CC=gcc
CFLAGS= -Wall -O2 -std=c99

.PHONY: all clean distclean

all: simul

msgconf.o: msgconf.c
	$(CC) $(CFLAGS) -c -o $@ $^

calcs.o: calcs.c
	$(CC) $(CFLAGS) -c -o $@ $^

inits.o: inits.c
	$(CC) $(CFLAGS) -c -o $@ $^

sync.o: sync.c
	$(CC) $(CFLAGS) -c -o $@ $^

loyal.o: loyal.c
	$(CC) $(CFLAGS) -c -o $@ $^

traitor.o: traitor.c
	$(CC) $(CFLAGS) -c -o $@ $^

ioconf.o: ioconf.c
	$(CC) $(CFLAGS) -c -o $@ $^

simul.o: simul.c
	$(CC) $(CFLAGS) -c -o $@ $^

simul: msgconf.o calcs.o inits.o sync.o loyal.o traitor.o ioconf.o simul.o
	$(CC) -o $@ $^

clean:
	$(RM) *.o

distclean: clean
	$(RM) simul
