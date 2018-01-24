#pragma once
#include "dbuffer.h"
#include "sbuffer.h"
#include <assert.h>
#include "tokens.h"
#include <stdio.h>
typedef struct {
	ureg line_number;
	char* filename;
    FILE* file;
    char* curr;
    dbuffer file_buffer;
    token lookahead_store[3]; //no more is ever needed
    token* lookahead_head;
}tokenizer;
typedef struct{
    dbuffer shy_ops;    //shunting yard operators
} parser;
typedef struct {
    dbuffer string_ptrs;
    sbuffer data_store;
    dbuffer ast;
    parser parsr;
    tokenizer tknzr;
}cunit;
