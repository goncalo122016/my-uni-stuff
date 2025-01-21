CC=gcc
CFLAGS= -Iinclude
LDFLAGS=

all: main

main: src/deque.o src/main.o
	$(CC) $(CFLAGS) src/deque.o src/main.o $(LDFLAGS) -o main

src/deque.o : src/deque.c

src/main.o : src/main.c

clean:
	rm -f src/*.o main 