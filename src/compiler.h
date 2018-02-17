#pragma once
#include "dbuffer.h"
#include "sbuffer.h"
#include "scopes.h"
#include "tokens.h"
#include "tokenizer.h"

typedef struct {
    char* path;
    dbuffer ast;
}cim_file;

typedef struct{
    ureg thread_count;
    dbuffer files;
    dbuffer parsers;
    tse_scope root_scope;
}cimc;

void cimc_init(cimc* c, ureg thread_count);
void cimc_fin(cimc* c);
void cimc_add_file(cimc* c, char* path);
void cimc_compile(cimc* c);
void cimc_dump_asts(cimc* c);
void cimc_print_stats(cimc* c);