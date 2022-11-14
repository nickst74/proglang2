#include "element.h"

#ifndef STACK_H
#define STACK_H

//#define STACK_SIZE 65536
// about 64MB in a 64-bit machine (32MB in 32-bit machine)
#define STACK_SIZE 4194304

struct Stack {
    int top;
    struct element* arr;
};

struct Stack* createStack();
void push(struct Stack* stack, struct element item);
struct element pop(struct Stack* stack);

#endif
