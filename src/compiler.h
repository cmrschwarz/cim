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
    dbuffer files;
    dbuffer parsers;
    tse_scope root_scope;
}cimc;
