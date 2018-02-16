#pragma once
#include "settings.h"
#include "types.h"
enum tse_type_t{
    TSE_FN_PTR,
    TSE_FUNCTION,
    TSE_STRUCT,
    TSE_GENERIC_STRUCT,
    TSE_SCOPE,
    TSE_TYPEDEF,
    TSE_PRIMITIVE,
};
enum tse_primitive_type_t{
    TSE_PRIMITIVE_U8,
    TSE_PRIMITIVE_U16 ,
    TSE_PRIMITIVE_U32,
    TSE_PRIMITIVE_U64,
    TSE_PRIMITIVE_S8,
    TSE_PRIMITIVE_S16,
    TSE_PRIMITIVE_S32,
    TSE_PRIMITIVE_S64,
};
#if DEBUG_ENUMS
    typedef enum tse_type_t tse_type;
    typedef enum tse_primitive_type_t tse_primitive_type;
#else
    typedef u8 tse_type;
    typedef u8 tse_primitive_type;
#endif

typedef struct tse_head_t{
    struct tse_head_t* next;
    tse_type type;
    u8 modifiers;

}tse_head;

typedef struct tse_primitive_t{
    tse_head head;
    tse_primitive_type primitive_type;
}tse_primitive;

typedef struct tse_typedef_t{
    tse_head head;

}tse_typedef;
typedef struct function_t{
    tse_head head;
    ureg arg_size;
    //followed by args
}function;


typedef struct tse_scope_t{
    tse_head head;

}tse_scope;