#pragma once
#include "types.h"
#include "dbuffer.h"

typedef struct token_t{
	u8 type;
	ureg str;
}token;

char* read_in_file(char* filename);

typedef struct cunit_t{
	//later on, this will read the file live, but this is easier for now
	char* str;
	char* pos;
	dbuffer string_store;
	dbuffer string_ptrs;
	//eventually, one ast per function?
	dbuffer ast;
    dbuffer shy_ops;    //shunting yard operators
}cunit;


void init(cunit* cu);
void get_token(cunit* cu, token* t);
void parse(cunit* cu, char* str);
void print_ast(cunit* cu);
