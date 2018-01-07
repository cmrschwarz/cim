#pragma once
#include "dbuffer.h"

typedef enum primitive_type_e{
    PRIMITIVE_TYPE_U8,
    PRIMITIVE_TYPE_U16,
    PRIMITIVE_TYPE_U32,
    PRIMITIVE_TYPE_U64,
    PRIMITIVE_TYPE_S8,
    PRIMITIVE_TYPE_S16,
    PRIMITIVE_TYPE_S32,
    PRIMITIVE_TYPE_S64,
    PRIMITIVE_TYPE_F8,
    PRIMITIVE_TYPE_F16,
    PRIMITIVE_TYPE_F32,
    PRIMITIVE_TYPE_F64,
}primitive_type;

typedef enum scope_type_e{
    SCOPE_TYPE_NSP,
    SCOPE_TYPE_BLOCK,
}scope_type;

typedef enum type_type_e{
    TYPE_TYPE_STRUCT,
    TYPE_TYPE_GENERIC_STRUCT,
    TYPE_TYPE_FUNCTION_PTR,
    TYPE_TYPE_PRIMITIVE,
    //seriously. this is the type of a template parameter of type type
    TYPE_TYPE_TYPE,
}type_type;

typedef struct scope_header_s{
    scope_type scope_type;
    struct scope_s* parent_scope;
}scope_header;

typedef struct se_type_s{
    char* name;
    type_type type;
    scope_header* defining_scope;
}se_type;
typedef struct bse_type_s{
    struct bse_type_s* prev;
    se_type val;
}bse_type;

typedef struct struct_member_s{
    char* name;
    se_type type;
}struct_member;
typedef struct se_type_struct_s{
    se_type type_header;
    struct struct_member* members_end;
    //this is followed by the se_struct_member's
}se_type_struct;

typedef struct generic_param_s{
    type_type type;
    char* name;
}generic_param;
typedef struct optional_generic_param_s{
    type_type type;
    char* name;
    union {
        type_type type;
        struct {
            dbuffer* ast;
            ureg ast_pos;
        }expr;
    } value;
}optional_generic_param;
typedef struct se_type_generic_struct_s{
    se_type type_header;
    generic_param* params_end;
    optional_generic_param* optional_params_end;
}se_type_generic_struct;
typedef struct se_type_generic_struct_instantation_s{
    //TODO
}se_generic_struct_instantation;

typedef struct se_variable_s{
    ureg name;
    se_type* type;
}se_variable;
typedef struct bse_variable_s{
    struct bse_variable_s* prev;
    se_variable val;
}bse_variable;

typedef struct function_parameter_s{
    se_type* type;
    char* name;
}function_parameter;
typedef struct se_function_s{
    char* name;
    function_parameter* parameter_end;
    //followed by arg_count se_type* 's
}se_function;
typedef struct bse_function_s{
    struct bse_function_s* prev;
    se_function val;
}bse_function;

typedef struct nsp_scope_s{
    scope_header header;
    dbuffer types;
    dbuffer variables;
    dbuffer functions;
    dbuffer scopes;
}nsp_scope;
typedef struct block_scope_s{
    scope_header header;
    bse_type* types;
    bse_variable* variables;
    bse_function* functions;
    scope_header* child_scopes;
}block_scope;

int block_scope_declare_struct(block_scope* s, bse_type* t);
se_type* block_scope_lookup_type(block_scope* s, char* name);


