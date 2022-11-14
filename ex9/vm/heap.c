#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "stack.h"
#include "heap.h"

#ifndef HEAP
#define HEAP

struct Heap* createHeap() {
    struct Heap* heap = (struct Heap*)malloc(sizeof(struct Heap));
    heap->arr = (struct heap_element*)malloc(HEAP_SIZE * sizeof(struct heap_element));
    heap->free_list = heap->arr;
    for(int i = 0; i < HEAP_SIZE-1; i++) {
        heap->arr[i].next = &heap->arr[i+1];
        heap->arr[i].marked = false;
    }
    heap->arr[HEAP_SIZE-1].next = NULL;
    heap->arr[HEAP_SIZE-1].marked = false;
    return heap;
}

void mark(struct heap_element* elem) {
    if(elem->marked == false) {
        elem->marked = true;
        struct cons_cell* pair = &elem->next->pair;
        if(pair != NULL) {
            if(pair->head.type == ADDR) {
                mark(pair->head.val.addr);
            }
            if(pair->tail.type == ADDR) {
                mark(pair->tail.val.addr);
            }
        }
    }
}

void sweep(struct Heap* heap) {
    heap->free_list = NULL;
    for(int i = HEAP_SIZE - 1; i >= 0; i--) {
        struct heap_element* elem = &heap->arr[i];
        if(elem->marked) {
            elem->marked = false;
        } else {
            elem->next = heap->free_list;
            heap->free_list = elem;
        }
    }
}

void run_gc(struct Stack* stack, struct Heap* heap) {
    int stack_size = stack->top;
    //printf("running gc, executed allocations: %li\n", allocs);
    // mark phase, examine reachability
    for(int i = 0; i < stack_size; i++) {
        struct element* se = &stack->arr[i];
        if(se->type == ADDR) {
            mark(se->val.addr);
        }
    }
    // sweep phase
    sweep(heap);
};

struct heap_element* alloc(struct Heap* heap, struct Stack* stack) {
    if(heap->free_list == NULL) {
        run_gc(stack, heap);
        //printf("Out of memory!\n");
        //exit(1);
    }
    struct heap_element* ret = heap->free_list;
    if(heap->free_list != NULL) {
        heap->free_list = heap->free_list->next;
    }
    return ret;
}


#endif