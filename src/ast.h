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
#define DEBUG_ENUMS 1

typedef uregh ast_rel_ptr;

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

typedef enum type_node_type_e{
    //TODO: maybe we need to make a resolved mask in here
    AST_TYPE_TYPE_SIMPLE,
    AST_TYPE_TYPE_SCOPED,
    AST_TYPE_TYPE_FN_PTR, //if it's referencing a fn_ptr, that looks like simple
    AST_TYPE_TYPE_GENERIC_STRUCT,
    AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT,
}type_node_type;


typedef enum expr_node_type_t{
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
}expr_node_type;

#if DEBUG_ENUMS
    enum ptrs_e{
        CIM___PTRS_DUMMY_ENUM
    };
#endif

typedef union ast_node_u{
    struct {
#       if DEBUG_ENUMS
            astnt astnt;
            operation _padding_;
#       else
            u8 astnt;
            u8 _padding_;
#       endif
        ast_rel_ptr size;
    }top_level_expr;
    struct {
#       if DEBUG_ENUMS
            expr_node_type type;
            operation _padding_;
#       else
            u8 type;
            u8 _padding_;
#       endif
        ast_rel_ptr size;
    }expr;
    struct {
#       if DEBUG_ENUMS
            expr_node_type expr_node_type;
            operation opcode;
#       else
            u8 expr_node_type;
            u8 opcode;
#       endif
        ast_rel_ptr size;
    }op;
    struct{
#       if DEBUG_ENUMS
            type_node_type type;
            //to restore identical memory layout to other structs in the union
            enum ptrs_e ptrs;
#       else
            u8 type;
            u8 ptrs;
#       endif
        ast_rel_ptr size;
    }type;
    char* str;
}ast_node;

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
    ast_rel_ptr size;
    ast_node tgt_type;
}astn_typedef;

typedef struct astn_function_call_t{
    u8 astnt;
    ureg arg_count;
}astn_function_call;





