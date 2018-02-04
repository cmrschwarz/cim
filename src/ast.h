#pragma once
#include "types.h"
#include "tokenizer.h"
#include "parser.h"
#include "scopes.h"
#define TO_U8(i)((u8)((i) & 0xFF))
#define TO_CHAR(i)((char)((i) & 0xFF))
#define TO_UREG(c)((ureg)(c))

#define DEBUG_ENUMS 0

typedef uregh ast_rel_ptr;

enum ast_node_type_e{
    ASTNT_EXPRESSION,
    ASTNT_FOR,
    ASTNT_TYPEDEF,
    ASTNT_ASSIGNMENT,
    ASTNT_VARIABLE_DECLARATION,
    ASTNT_VARIABLE_DECLARATION_AMBIGUOUS,
    ASTNT_FUNCTION_DECLARATION,
    ASTNT_GENERIC_FUNCTION_DECLARATION,
    //@PERF: consider adding function call to avoid expr wrapper
};


enum expr_node_type_t{
    EXPR_NODE_NUMBER = TOKEN_NUMBER,
    EXPR_NODE_LITERAL = TOKEN_LITERAL,
    EXPR_NODE_BINARY_LITERAL = TOKEN_BINARY_LITERAL,
    EXPR_NODE_VARIABLE ,
    EXPR_NODE_CANCER_PTRS ,
    EXPR_NODE_OP_LR,
    EXPR_NODE_OP_L,
    EXPR_NODE_OP_R,
    EXPR_NODE_CAST,
    EXPR_NODE_FN_CALL,
    EXPR_NODE_ARRAY_ACCESS,
    EXPR_NODE_GENERIC_FN_CALL,
    EXPR_NODE_NOP,

    EXPR_NODE_TYPE_PARAM,
    EXPR_NODE_TYPE_FN_PTR,
    EXPR_NODE_TYPE_GENERIC_STRUCT_INST,
    EXPR_NODE_TYPE_GENERIC_STRUCT_DEF,
    EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS,
    EXPR_NODE_TYPE_ARRAY,
    EXPR_NODE_TYPE_SCOPED,
    EXPR_NODE_PAREN,   //temp for shunting yard
    EXPR_NODE_TYPE_SIMPLE = EXPR_NODE_VARIABLE,
};

#if DEBUG_ENUMS
    typedef enum ast_node_type_e ast_node_type;
    typedef enum expr_node_type_t expr_node_type;
    typedef enum operation_e operation;
#else
    typedef u8 ast_node_type;
    typedef u8 expr_node_type;
    typedef u8 operation;
#endif

typedef union ast_node_u{
    struct {
        ast_node_type type;
        operation __padding;
        ast_rel_ptr size;
    }common;
    struct {
        expr_node_type type;
        union {
            operation opcode;
            u8 ptrs;
            u8 modifiers;
        }special;
        ast_rel_ptr size;
    }expr;
    struct{
        expr_node_type type;
        u8 ptrs;
        ast_rel_ptr size;
    }type;
    struct {
        ast_node_type type;
        bool assigning;
        ast_rel_ptr size;
    }var_decl;
    struct {
        ast_node_type type;
        bool assigning;
        ast_rel_ptr size;
    }for_stmt;
    char* str;
    ureg full_size;
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





