#pragma once
#include "compiler.h"
#include "tokenizer.h"
#include "operations.h"
typedef enum modifier_e{
    MOD_PUBLIC,
    MOD_PRIVATE,
    MOD_CONST,
    MOD_INLINE,
}modifier;

void cunit_init(cunit* cu);
void cunit_fin(cunit* cu);

void parse_file(cunit* cu, char* filename);
void print_ast(cunit* cu);
