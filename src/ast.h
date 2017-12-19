#include "types.h"

//ast stands for abstract syntax tree
//astn stands for abstract syntax tree node
//astnt stands for abstract systax tree node type
//all nodes are at least aligned to a sizeof(ureg) byte boundary

typedef enum astnts_t{
    ASTNT_NUMBER,
    ASTNT_VARIABLE,
    ASTNT_DECLARATION,
    ASTNT_EXPRESSION,
    ASTNT_FUNCTION_DECLARATION,
}astnts;
typedef struct astn_assignment_t{
    u8 astnt;
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

//operators are used in expressions. 
//These are backwards in the ast because of shunting yard
//Therefore, they need to have constant size and are accessed through a union
//They don't need to have a astnt because that is in the union struct
typedef struct expr_op_lr_t{
    char op;
}expr_op_lr;

typedef struct expr_op_r_t{
    char op;
}expr_op_r;

typedef struct expr_op_l_t{
    char op;
}expr_op_l;

typedef struct expr_fn_call_t{
    ureg arg_count;
}expr_fn_call;

typedef struct expr_number{
    ureg str;
}expr_number;

typedef struct expr_elem_t{
   u8 astnt;
    union {
        expr_op_lr op_lr;
        expr_op_r op_r;
        expr_op_l op_l;
        expr_fn_call fn_call;
        expr_number number;  
    }el;
}expr_elem;




