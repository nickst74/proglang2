#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "stack.h"


#ifndef STACK
#define STACK

struct Stack* createStack() {
    struct Stack* stack = (struct Stack*)malloc(sizeof(struct Stack));
    stack->top = -1;
    stack->arr = (struct element*)malloc(STACK_SIZE * sizeof(struct element));
    return stack;
}

bool isFull(struct Stack* stack) {
    return stack->top == STACK_SIZE - 1;
}

bool isEmpty(struct Stack* stack)
{
    return stack->top == -1;
}

void push(struct Stack* stack, struct element item) {
    if (isFull(stack)) {
        printf("Stack overflow occured\n");
        exit(-1);
    }
    stack->arr[++stack->top] = item;
}

struct element pop(struct Stack* stack) {
    if (isEmpty(stack)) {
        printf("Illegal operation occured: pop from empty stack\n");
        exit(-1);
    }
    return stack->arr[stack->top--];
}

#endif
