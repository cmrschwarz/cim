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
enum modifiers_e{
    MOD_CONST = 1,
    MOD_PUBLIC = 2,
    MOD_PRIVATE = 4,
    MOD_STATIC = 8,
}modifiers;
enum astn_type_e{
    ASTNT_EXPRESSION,
    ASTNT_FREE_BLOCK,
    ASTNT_NAMESPACE,
    ASTNT_IF,
    ASTNT_IF_ELSE,
    ASTNT_FOR,
    ASTNT_WHILE,
    ASTNT_LOOP,
    ASTNT_NAMED_LOOP,
    ASTNT_TYPEDEF,
    ASTNT_ASSIGNMENT,
    ASTNT_VARIABLE_DECLARATION,
    ASTNT_VARIABLE_DECLARATION_AMBIGUOUS,
    ASTNT_ASSIGNING_VARIABLE_DECLARATION,
    ASTNT_ASSIGNING_VARIABLE_DECLARATION_AMBIGUOUS,
    ASTNT_FUNCTION_DECLARATION,
    ASTNT_GENERIC_FUNCTION_DECLARATION,
    ASTNT_STRUCT_DECLARATION,
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
    EXPR_NODE_GENERIC_FN_CALL,
    EXPR_NODE_ARRAY_ACCESS,
    EXPR_NODE_NOP,

    EXPR_NODE_TYPE_PARAM,
    EXPR_NODE_TYPE_FN_PTR,
    EXPR_NODE_TYPE_GENERIC_STRUCT_INST,
    EXPR_NODE_TYPE_GENERIC_STRUCT_DECL,
    EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS,

    EXPR_NODE_TYPE_SCOPED,
    EXPR_NODE_TYPE_PTR,
    EXPR_NODE_PAREN,   //temp for shunting yard
    EXPR_NODE_TYPE_ARRAY = EXPR_NODE_ARRAY_ACCESS,
    EXPR_NODE_TYPE_SIMPLE = EXPR_NODE_VARIABLE,
};


#if DEBUG_ENUMS
    typedef enum astn_type_e astn_type;
    typedef enum expr_node_type_t expr_node_type;
    typedef enum operation_e operation;
    typedef u8 _padding_;
#else
    typedef u8 astn_type;
    typedef u8 expr_node_type;
    typedef u8 operation;
    typedef u8 _padding_;
#endif

typedef union astn_u{
    struct {
        astn_type type;
        _padding_ _padding_;
        ast_rel_ptr size;
    }common;
    struct {
        expr_node_type type;
        union {
            operation opcode;
            u8 modifiers;
        }special;
        ast_rel_ptr size;
    }expr;
    struct{
        expr_node_type type;
        u8 mods;
        ast_rel_ptr size;
    }type;
    char* str;
    ureg full_size;
}astn;


