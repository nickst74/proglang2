.PHONY: clean 

TARGET = vm
CC=gcc 
CFLAGS= -g -O3

default: $(TARGET)
all: default

stack.o: stack.c stack.h element.h
	$(CC) $(CFLAGS) -c $< -o $@

heap.o: heap.c heap.h stack.h element.h
	$(CC) $(CFLAGS) -c $< -o $@

vm: vm.c opcode.h stack.o heap.o
	$(CC) $(CFLAGS) $< -o $@

clean:
	-rm -f *.o

distclean:
	-rm -f *.o
	-rm -f $(TARGET)
