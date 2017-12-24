#pragma once
#include "types.h"

#define TO_U8(i)((u8)((i) & 0xFF))
#define TO_CHAR(i)((char)((i) & 0xFF))

//ast stands for abstract syntax tree
//astn stands for abstract syntax tree node
//astnt stands for abstract systax tree node type
//all nodes are at least aligned to a sizeof(ureg) byte boundary

typedef enum astnt_t {
    ASTNT_NUMBER = 0,
    ASTNT_VARIABLE = 1,
    ASTNT_DECLARATION = 2,
    ASTNT_EXPRESSION = 3,
    ASTNT_FUNCTION_DECLARATION = 4,
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

typedef struct astn_variable_t{
    u8 astnt;
    ureg name;
}astn_variable;
typedef struct astn_number_t{
    u8 astnt;
    ureg number_str;
}astn_number;
typedef struct astn_function_call_t{
    u8 astnt;
    ureg arg_count;
}astn_function_call;
typedef struct astn_expression_t{
    u8 astnt;
    ureg end;
}astn_expression;
//operators are used in expressions. 
//These are backwards in the ast because of shunting yard
//Therefore, they need to have constant size and are accessed through a union
//They don't need to have a astnt because that is in the union struct

//we specify values to ensure it fits inside a u8
typedef enum expr_elem_type_t{
    EXPR_ELEM_TYPE_NUMBER = 0,
    EXPR_ELEM_TYPE_VARIABLE = 1,
    EXPR_ELEM_TYPE_OP_LR = 2,
    EXPR_ELEM_TYPE_OP_L = 3,
    EXPR_ELEM_TYPE_OP_R = 4,
    EXPR_ELEM_TYPE_UNARY = 5,
    EXPR_ELEM_TYPE_BRACE = 6,   //closing will never be stored
    EXPR_ELEM_TYPE_FN_CALL = 7,   //closing will never be stored
}expr_elem_type;


typedef struct expr_elem_t{
    u8 type;
    u8 op;
    ureg val;
}expr_elem;




