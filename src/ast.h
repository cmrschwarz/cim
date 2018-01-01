#pragma once
#include "types.h"
#include "tokenizer.h"
#define TO_U8(i)((u8)((i) & 0xFF))
#define TO_CHAR(i)((char)((i) & 0xFF))
#define TO_UREG(c)((ureg)(c))
//ast stands for abstract syntax tree
//astn stands for abstract syntax tree node
//astnt stands for abstract systax tree node type
//all nodes are at least aligned to a sizeof(ureg) byte boundary
typedef struct scope_s{
    dbuffer functions;
    dbuffer generic_functions;
    dbuffer variables;
    dbuffer types;
}scope;

typedef enum astnt_e{
    //used instead of a single element expression
    ASTNT_NUMBER = TOKEN_NUMBER,
    ASTNT_LITERAL = TOKEN_LITERAL,
    ASTNT_BINARY_LITERAL = TOKEN_BINARY_LITERAL,
    ASTNT_VARIABLE,
    ASTNT_EXPRESSION,
    ASTNT_ASSIGNMENT,
    ASTNT_DECLARATION,
    ASTNT_FUNCTION_DECLARATION,
}astnt;


typedef struct astn_assignment_t{
    u8 astnt;
    u16 ptrs;
}astn_assignment;

typedef struct astn_declaration_t{
    u8 astnt;
    bool assigning;
    u16 ptrs;
    ureg type;
    ureg name;
}astn_declaration;

typedef struct astn_function_call_t{
    u8 astnt;
    ureg arg_count;
}astn_function_call;
//operators are used in expressions. 
//These are backwards in the ast because of shunting yard
//Therefore, they need to have constant size and are accessed through a union
//They don't need to have a astnt because that is in the union struct

//we specify values to ensure it fits inside a u8
typedef enum expr_elem_type_t{
    EXPR_ELEM_TYPE_NUMBER = TOKEN_NUMBER,
    EXPR_ELEM_TYPE_LITERAL = TOKEN_LITERAL,
    EXPR_ELEM_TYPE_BINARY_LITERAL = TOKEN_BINARY_LITERAL,
    EXPR_ELEM_TYPE_VARIABLE = ASTNT_VARIABLE,
    EXPR_ELEM_TYPE_EXPR = ASTNT_EXPRESSION,
    EXPR_ELEM_TYPE_OP_LR,
    EXPR_ELEM_TYPE_OP_L,
    EXPR_ELEM_TYPE_OP_R,
    EXPR_ELEM_TYPE_UNARY,
    EXPR_ELEM_TYPE_PAREN,   //closing will never be stored
    EXPR_ELEM_TYPE_FN_CALL,
    EXPR_ELEM_TYPE_FN_NAME,
    EXPR_ELEM_TYPE_ARRAY_ACCESS,
    EXPR_ELEM_TYPE_ARRAY_NAME,
    EXPR_ELEM_TYPE_GENERIC_FN_GENERIC_ARGS,
    EXPR_ELEM_TYPE_GENERIC_FN_ARGS,
    EXPR_ELEM_TYPE_GENERIC_FN_NAME,
    EXPR_ELEM_TYPE_GENERIC_FN_CALL,
}expr_elem_type;

typedef struct expr_elem_t{
    u8 type;
    //expr_elem_type type; //for debugging purposes
    u8 op;
    ureg val;
}expr_elem;





