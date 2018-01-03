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
typedef enum var_type_e{
    VAR_TYPE_UNRESOLVED,
    VAR_TYPE_PRIMITIVE,
    VAR_TYPE_STRUCT,
    VAR_TYPE_GENERIC_STRUCT,
}var_type;
typedef struct se_typedef_s{
    ureg name;
    ureg type;
}se_typedef;
typedef struct se_struct{
    ureg name;
    ureg ast_pos;
}se_struct;
typedef struct se_variable_s{
    ureg name;
    var_type type;
    union type{
        ureg unresolved_str;
        ureg nsp_scope_struct;
        se_struct* block_scope_struct;
        primitive_type primitive_type;
    };
}se_variable;
typedef struct se_function_s{
    ureg name;
    ureg ast_pos;
}se_function;

typedef enum scope_type_e{
    SCOPE_TYPE_NSP,
    SCOPE_TYPE_BLOCK,
}scope_type;
typedef struct nsp_scope_s{
    scope_type scope_type;
    void* parent_scope;
    dbuffer structs;
    dbuffer typedefs;
    dbuffer vars;
    dbuffer functions;
    dbuffer scopes;
}nsp_scope;

typedef struct block_scope_s{
    scope_type scope_type;
    void*  parent_scope;

}block_scope;

