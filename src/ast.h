#pragma once
#include "types.h"
#include "tokenizer.h"
#include "parser.h"
#include "scopes.h"
#define TO_U8(i)((u8)((i) & 0xFF))
#define TO_CHAR(i)((char)((i) & 0xFF))
#define TO_UREG(c)((ureg)(c))

#define DEBUG_ENUMS 1

typedef uregh ast_rel_ptr;

enum ast_node_type_e{
    //used instead of a single element expression
    ASTNT_NUMBER = TOKEN_NUMBER,
    ASTNT_LITERAL = TOKEN_LITERAL,
    ASTNT_BINARY_LITERAL = TOKEN_BINARY_LITERAL,
    ASTNT_VARIABLE,
    ASTNT_TYPEDEF,
    ASTNT_EXPRESSION,
    ASTNT_ASSIGNMENT,
    ASTNT_VARIABLE_DECLARATION,
    ASTNT_FUNCTION_DECLARATION,
    //@PERF: consider adding function call to avoid expr wrapper
};

enum type_node_type_e{
    //TODO: maybe we need to make a resolved mask in here
    AST_TYPE_TYPE_SIMPLE,
    AST_TYPE_TYPE_SCOPED,
    AST_TYPE_TYPE_FN_PTR, //if it's referencing a fn_ptr, that looks like simple
    AST_TYPE_TYPE_GENERIC_STRUCT,
    AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT,
    AST_TYPE_TYPE_ARRAY,
};


enum expr_node_type_t{
    EXPR_NODE_TYPE_NUMBER = TOKEN_NUMBER,
    EXPR_NODE_TYPE_LITERAL = TOKEN_LITERAL,
    EXPR_NODE_TYPE_BINARY_LITERAL = TOKEN_BINARY_LITERAL,
    EXPR_NODE_TYPE_VARIABLE = ASTNT_VARIABLE,
    EXPR_NODE_TYPE_EXPR = ASTNT_EXPRESSION,
    EXPR_NODE_TYPE_OP_LR,
    EXPR_NODE_TYPE_OP_L,
    EXPR_NODE_TYPE_OP_R,
    EXPR_NODE_TYPE_UNARY,
    EXPR_NODE_TYPE_PAREN,   //closing will never be stored
    EXPR_NODE_TYPE_FN_CALL,
    EXPR_NODE_TYPE_ARRAY_ACCESS,
    EXPR_NODE_TYPE_GENERIC_FN_CALL,
};


#if DEBUG_ENUMS
    typedef enum ast_node_type_e ast_node_type;
    typedef enum type_node_type_e type_node_type;
    typedef enum expr_node_type_t expr_node_type;
    typedef enum operation_e operation;
#else
    typedef u8 ast_node_type;
    typedef u8 type_node_type;
    typedef u8 expr_node_type;
    typedef u8 operation;
#endif

typedef union ast_node_u{
    //ast_expr, sub_expr and op must have identical memory layout
    //because flush_shy_op just uses sub_expr.size for all three
    struct {
        ast_node_type type;
        operation _padding_;
        ast_rel_ptr size;
    }ast_expr;
    struct {
        expr_node_type type;
        operation _padding_;
        ast_rel_ptr size;
    }sub_expr;
    struct {
        expr_node_type node_type; //avoiding confusion with opcode
        operation opcode;
        ast_rel_ptr size;
    }op;
    struct{
        type_node_type type;
        u8 ptrs;
        ast_rel_ptr size;
    }type;
    struct {
        ast_node_type type;
        ast_rel_ptr size;
    }type_define;
    struct {
        ast_node_type type;
        bool assigning;
        ast_rel_ptr size;
    }var_decl;
    char* str;
    ureg size;
}ast_node;

typedef struct astn_assignment_t{
    ast_node_type type;
    u16 ptrs;
}astn_assignment;

typedef struct astn_typedef_s{
    ast_node_type type;
    ast_rel_ptr size;
    ast_node tgt_type;
}astn_typedef;

typedef struct astn_function_call_t{
    ast_node_type type;
    ureg arg_count;
}astn_function_call;





