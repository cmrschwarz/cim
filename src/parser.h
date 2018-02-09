#pragma once
#include "compiler.h"
#include "tokenizer.h"
#include "operations.h"

void cunit_init(cunit* cu);
void cunit_fin(cunit* cu);

void parse_file(cunit* cu, char* filename);
void print_ast(cunit* cu);
