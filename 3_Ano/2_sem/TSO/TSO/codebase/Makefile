CC=gcc
CFLAGS= -Wall -DHAVE_UTIMENSAT
DEPS = metaindex.h pagecache.h
OBJ = passthrough.o metaindex.o pagecache.o

LIBS=`pkg-config --cflags --libs glib-2.0` `pkg-config fuse3 --cflags --libs` -lpthread -lbz2 -lssl -lcrypto

all: passthrough passthrough_normal

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS) $(LIBS)

passthrough: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

passthrough_normal: passthrough_normal.c
	$(CC) $< -o $@ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f *.o passthrough passthrough_normal