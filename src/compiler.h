#pragma once
#include "dbuffer.h"
#include "sbuffer.h"
#include <assert.h>
#include "token_strings.h"
#include <stdio.h>
#include "tokenizer_t.h"

typedef struct{
    dbuffer shy_ops;    //shunting yard operators
} parser;

typedef struct {
    sbuffer data_store;
    dbuffer ast;
    parser parsr;
    tokenizer tknzr;
}cunit;
