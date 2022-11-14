// Stamatelopoulos Nikolaos
// 03116138

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "stack.c"
#include "heap.c"
#include "opcode.h"

#define MAX_CODE_SIZE 65536

#ifndef __GNUC__
#define NEXT_INSTRUCTION goto next_instruction
#else
#define NEXT_INSTRUCTION goto *(void *)(label_tab[code[pc]])
#endif

int main(int argc, char const *argv[]) {


    #ifdef __GNUC__
    static void *label_tab[] = {
        &&l_halt,   //0x00
        &&l_jump,   //0x01
        &&l_jnz,    //0x02
        &&l_dup,    //0x03
        &&l_swap,   //0x04
        &&l_drop,   //0x05
        &&l_push4,  //0x06
        &&l_push2,  //0x07
        &&l_push1,  //0x08
        &&l_add,    //0x09
        &&l_sub,    //0x0a
        &&l_mul,    //0x0b
        &&l_div,    //0x0c
        &&l_mod,    //0x0d
        &&l_eq,     //0x0e
        &&l_ne,     //0x0f
        &&l_lt,     //0x10
        &&l_gt,     //0x11
        &&l_le,     //0x12
        &&l_ge,     //0x13
        &&l_not,    //0x14
        &&l_and,    //0x15
        &&l_or,     //0x16
        &&l_input,  //0x17
        &&l_output, //0x18
        &&l_default,//0x19
        &&l_default,//0x1a
        &&l_default,//0x1b
        &&l_default,//0x1c
        &&l_default,//0x1d
        &&l_default,//0x1e
        &&l_default,//0x1f
        &&l_default,//0x20
        &&l_default,//0x21
        &&l_default,//0x22
        &&l_default,//0x23
        &&l_default,//0x24
        &&l_default,//0x25
        &&l_default,//0x26
        &&l_default,//0x27
        &&l_default,//0x28
        &&l_default,//0x29
        &&l_clock,  //0x2a
        &&l_default,//0x2b
        &&l_default,//0x2c
        &&l_default,//0x2d
        &&l_default,//0x2e
        &&l_default,//0x2f
        &&l_cons,   //0x30
        &&l_hd,     //0x31
        &&l_tl      //0x32
    };
    #endif

    if(argc != 2) {
        printf("Please provide program file's name as argument.\n");
        exit(0);
    }
    FILE *fp = fopen(argv[1], "r");
    if(fp == NULL) {
        printf("Could not open file.\n");
        exit(1);
    }
    unsigned char code[MAX_CODE_SIZE] = { '\0' };
    unsigned char c;
    int n = 0;
    while(fscanf(fp, "%c", &c) != EOF && n < MAX_CODE_SIZE) {
        code[n++] = c;
    }
    fclose(fp);

    // init stack and heap
    struct Stack* stack = createStack();
    struct Heap* heap = createHeap();

    //printf("heap element: %i\n", sizeof(struct heap_element));
    //printf("stack element: %i\n", sizeof(struct element));
    // init clock and program counter
    clock_t started = clock();
    u_int16_t pc = 0;

    // parsing code
    while(1) {
        next_instruction:;
        if(pc >= n || pc >= MAX_CODE_SIZE) {
            printf("Program counter out of bounds, halting execution.\n");
            exit(0);
        }
        unsigned char opcode = code[pc];
        switch (opcode) {
            case _halt: {
                l_halt:;
                return 0;
            }        
            case _jump: {
                l_jump:;
                pc = code[pc+1] + (code[pc+2] << 8);
                NEXT_INSTRUCTION;
                break;
            }
            case _jnz: {
                l_jnz:;
                if(pop(stack).val.integer != 0) {
                    pc = code[pc+1] + (code[pc+2] << 8);
                } else {
                    pc = pc + 3;
                }
                NEXT_INSTRUCTION;
                break;
            }
            case _dup: {
                l_dup:;
                u_int index = code[pc+1];
                //** check for out of bounds **
                struct element copy = stack->arr[stack->top - index];
                push(stack, copy);
                pc += 2;
                NEXT_INSTRUCTION;
                break;
            }
            case _swap: {
                l_swap:;
                u_int index = code[pc+1];
                //** check for out of bounds **
                struct element tmp = stack->arr[stack->top - index];
                stack->arr[stack->top - index] = stack->arr[stack->top];
                stack->arr[stack->top] = tmp;
                pc += 2;
                NEXT_INSTRUCTION;
                break;
            }
            case _drop: {
                l_drop:;
                pop(stack);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _push4: {
                l_push4:;
                struct element elem;
                elem.val.integer = (int32_t)(code[pc+1] + (code[pc+2] << 8) + (code[pc+3] << 16) + (code[pc+4] << 24));
                elem.type = INT;
                push(stack, elem);
                pc += 5;
                NEXT_INSTRUCTION;
                break;
            }
            case _push2: {
                l_push2:;
                struct element elem;
                elem.val.integer = (int16_t)(code[pc+1] + (code[pc+2] << 8));
                elem.type = INT;
                push(stack, elem);
                pc += 3;
                NEXT_INSTRUCTION;
                break;
            }
            case _push1: {
                l_push1:;
                struct element elem;
                elem.val.integer = (int8_t)(code[pc+1]);
                elem.type = INT;
                push(stack, elem);
                pc += 2;
                NEXT_INSTRUCTION;
                break;
            }
            case _add: {
                l_add:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a + b;
                result.type = INT;
                push(stack, result);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _sub: {
                l_sub:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a - b;
                result.type = INT;
                push(stack, result);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _mul: {
                l_mul:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a * b;
                result.type = INT;
                push(stack, result);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _div: {
                l_div:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a / b;
                result.type = INT;
                push(stack, result);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _mod: {
                l_mod:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a % b;
                result.type = INT;
                push(stack, result);
                pc ++;
                NEXT_INSTRUCTION;
                break;
            }
            case _eq: {
                l_eq:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a == b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _ne: {
                l_ne:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a != b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _lt: {
                l_lt:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a < b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _gt: {
                l_gt:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a > b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _le: {
                l_le:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a <= b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _ge: {
                l_ge:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a >= b ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _not: {
                l_not:;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a == 0 ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _and: {
                l_and:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a != 0 && b != 0 ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _or: {
                l_or:;
                int b = pop(stack).val.integer;
                int a = pop(stack).val.integer;
                struct element result;
                result.val.integer = a != 0 || b != 0 ? 1 : 0;
                result.type = INT;
                push(stack, result);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _input: {
                l_input:;
                unsigned char input = getchar();
                struct element elem;
                elem.val.integer = input;
                elem.type = INT;
                push(stack, elem);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _output: {
                l_output:;
                unsigned char output = pop(stack).val.integer;
                printf("%c", output);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _clock: {
                l_clock:;
                printf("%0.6lf\n", (double) (clock()-started) / CLOCKS_PER_SEC);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _cons: {
                l_cons:;
                struct element b = pop(stack);
                struct element a = pop(stack);
                struct heap_element* h_elem = alloc(heap, stack);
                if(h_elem == NULL) {
                    printf("Out of heap memory. Maybe try increasing heap size in \"heap.h\" file.");
                    exit(1);
                }
                h_elem->pair.head = a;
                h_elem->pair.tail = b;
                struct element elem;
                elem.val.addr = h_elem;
                elem.type = ADDR;
                push(stack, elem);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _hd: {
                l_hd:;
                struct element h_elem = pop(stack);
                struct element elem;
                elem = h_elem.val.addr->pair.head;
                push(stack, elem);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            case _tl: {
                l_tl:;
                struct element h_elem = pop(stack);
                struct element elem;
                elem = h_elem.val.addr->pair.tail;
                push(stack, elem);
                pc++;
                NEXT_INSTRUCTION;
                break;
            }
            default: {
                l_default:;
                printf("Unrecognised opcode found: \'%c\'", opcode);
                NEXT_INSTRUCTION;
                break;
            }
        }
    }
    return 0;
}
