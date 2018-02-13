#pragma once
#include "types.h"
typedef enum tse_type_t{
    TSE_FN_PTR,
    TSE_FUNCTION,
    TSE_STRUCT,
    TSE_GENERIC_STRUCT,
    TSE_SCOPE,
    TSE_TYPEDEF,
    TSE_PRIMITIVE,
}tse_type;
typedef enum tse_primitive_type_t{
    TSE_PRIMITIVE_U8,
    TSE_PRIMITIVE_U16,
    TSE_PRIMITIVE_U32,
    TSE_PRIMITIVE_U64,
    TSE_PRIMITIVE_S8,
    TSE_PRIMITIVE_S16,
    TSE_PRIMITIVE_S32,
    TSE_PRIMITIVE_S64,
}tse_primitive_type;
typedef struct tse_head_t{
    char* name;
    tse_type type;
}tse_head;

typedef struct tse_primitive_t{
    tse_head head;
    enum tse_primitive_type_t primitive_type;
}tse_primitive;

typedef struct function_t{
    tse_head head;
    ureg arg_size;
    //followed by args
}function;

