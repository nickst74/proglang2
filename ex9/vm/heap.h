#include <stdbool.h>

#include "element.h"

#ifndef HEAP_H
#define HEAP_H

// something smaller than 512MB (if in 64bit machine, else half of that size for 32-bit)
#define HEAP_SIZE 8388608

struct Heap {
    struct heap_element* free_list;
    struct heap_element* arr;
};

struct Heap* createHeap();
struct heap_element* alloc(struct Heap* heap, struct Stack* stack);

#endif
