CC=gcc
CFLAGS=
INCPATHS=../some_other_project/
LIBPATHS=../yet_another_project/
LINKER=gcc -o

EXEC=run
BINARY=prog2
LLSOURCES=$(wildcard *.ll)
ASMSOURCES=$(wildcard *.s)
OBJECTS=$(ASMSOURCES:.s=.o)

all: $(ASMSOURCES) $(BINARY)
 
$(BINARY): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@
 
.c.o:
	$(CC) $(INCFLAGS) $(CFLAGS) -fPIC $< -o $@
 
distclean: clean
	rm -f $(BINARY)
 
clean:
	rm -f $(OBJECTS) $(ASMSOURCES)
