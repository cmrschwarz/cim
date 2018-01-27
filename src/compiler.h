#pragma once
#include "dbuffer.h"
#include "sbuffer.h"
#include <assert.h>
#include "tokens.h"
#include <stdio.h>
typedef struct {
    ureg line_number;
    ureg line_column;
    ureg line_comment_offset;
}looahead_line_backup;
typedef struct {
	ureg line_number;
    char* line_start;
    ureg unloaded_columns_of_line;
	ureg line_comment_offset;
	char* filename;
    FILE* file;
    char* curr;
    dbuffer file_buffer;
    looahead_line_backup line_backup[3];
    token lookahead_store[3]; //no more is ever needed
    int lookahead_count;
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
    ureg lah;
    ureg cons;
}cunit;
