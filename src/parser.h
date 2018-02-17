#pragma once
#include "compiler.h"
#include "tokenizer.h"

typedef struct {
    dbuffer* ast;
    dbuffer shy_ops;
    tokenizer tk;
}parser;

void parser_init(parser* p);
void parser_fin(parser* p);

void parse_file(parser* p, cim_file* f);
