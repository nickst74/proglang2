#ifndef ELEM
#define ELEM

union u_elem {
    int integer;
    struct heap_element* addr;
};

enum type {
    INT,
    ADDR
};

struct element {
    union u_elem val;
    enum type type;
};

struct cons_cell {
    struct element head, tail;
};

struct heap_element
{
    struct cons_cell pair;
    struct heap_element* next;
    bool marked;
};


#endif
