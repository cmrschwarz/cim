#pragma once
#include "compiler.h"
#include "tokenizer.h"
#include "operations.h"
enum modifier_e{
    MOD_PUBLIC,
    MOD_PRIVATE,
    MOD_CONST,
    MOD_INLINE,
}modifier;

void cunit_init(cunit* cu);
void cunit_fin(cunit* cu);

char* read_in_file(char* filename);
void parse(cunit* cu, char* str);
void print_ast(cunit* cu);
