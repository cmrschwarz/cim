#include "types.h"

//ast stands for abstract syntax tree
//astn stands for abstract syntax tree node
//astnt stands for abstract systax tree node type
//all nodes are at least aligned to a sizeof(ureg) byte boundary

typedef enum astnt_t{
    ASTNT_NUMBER,
    ASTNT_VARIABLE,
    ASTNT_DECLARATION,
    ASTNT_EXPRESSION,
    ASTNT_FUNCTION_DECLARATION,
}astnt;
typedef struct astn_assignment_t{
    astnt astnt;
}astn_assignment;

typedef struct astn_declaration_t{
    astnt astnt;
    bool assigning;
    u16 ptrs;
    ureg type;
    ureg name;
}astn_declaration;

typedef struct astn_variable_t{
    astnt astnt;
    ureg name;
}astn_variable;
typedef struct astn_number_t{
    astnt astnt;
    ureg number_str;
}astn_number;
typedef struct astn_function_call_t{
    astnt astnt;
    ureg arg_count;
}astn_function_call;
typedef struct astn_expression_t{
    astnt astnt;
    ureg end;
}astn_expression;
//operators are used in expressions. 
//These are backwards in the ast because of shunting yard
//Therefore, they need to have constant size and are accessed through a union
//They don't need to have a astnt because that is in the union struct
typedef enum expr_elem_type_t{
    EXPR_ELEM_TYPE_NUMBER,
    EXPR_ELEM_TYPE_OP_LR,
    EXPR_ELEM_TYPE_OP_L,
    EXPR_ELEM_TYPE_OP_R,
}expr_elem_type;


typedef struct expr_elem_t{
    u8 type;
    u8 op;
    union {
        ureg op_rend;
        ureg number_str;
    }val;
}expr_elem;




