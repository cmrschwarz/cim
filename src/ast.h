#pragma once
#include "types.h"
#include "tokenizer.h"
#include "parser.h"
#include "scopes.h"
#define TO_U8(i)((u8)((i) & 0xFF))
#define TO_CHAR(i)((char)((i) & 0xFF))
#define TO_UREG(c)((ureg)(c))
//ast stands for abstract syntax tree
//astn stands for abstract syntax tree node
//astnt stands for abstract systax tree node type
//all nodes are at least aligned to a sizeof(ureg) byte boundary

typedef uregh ast_rel_ptr;

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
    ASTNT_TYPEDEF,
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
    char* type;
    char* name;
}astn_declaration;
typedef struct astn_typedef_s{
    u8 astnt;
    union {
        char* str;
        se_type* type;
    }def;
    ast_rel_ptr type_end;
}astn_typedef;

typedef struct astn_function_call_t{
    u8 astnt;
    ureg arg_count;
}astn_function_call;

enum ast_type_type{
    //TODO: maybe we need to make a resolved mask in here
    AST_TYPE_TYPE_SIMPLE,
    AST_TYPE_TYPE_SCOPED,
    AST_TYPE_TYPE_FN_PTR,
    AST_TYPE_TYPE_GENERIC_STRUCT,
    AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT,
};
//structure:
//  normal type: 1 type node
//  scoped type: 1 type node + x type nodes for scope
//  function pointer: 1 type node + 1 astn_type for ret val + x astn_type's for params
//  generic struct: 1 type node + x astn_types for params
//  scoped generic struct: 1 type node + 1 type node for scopes end + x type nodes for scope
//                         + x astn_types for params
typedef union astn_type_node_u{
    se_type* resolved_type;
    char* str;
    ast_rel_ptr scoped_generic_struct_end;
}astn_type_node;
typedef struct astn_type_s{
    u8 type;
    u8 ptrs;
    ast_rel_ptr end;
}astn_type;


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
    EXPR_ELEM_TYPE_ARRAY_ACCESS,
    EXPR_ELEM_TYPE_GENERIC_FN_CALL,
}expr_elem_type;


typedef union expr_elem_s{
#if 0
    struct{
        expr_elem_type type;
        enum OP op;
        ast_rel_ptr nest_size;
    }id;
#else
    struct{
        u8 type;
        u8 op;
        //number of expr elements until end of op args
        ast_rel_ptr nest_size; //DECIDE: maybe use u32 instead here
    }id;
#endif
    char* str;
    ureg ast_pos;
}expr_elem;






