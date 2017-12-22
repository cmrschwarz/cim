#pragma once
#include "types.h"
#include "dbuffer.h"
#define OPERATOR_DOUBLE(c)((u8)(c) + 128)
#define OPERATOR_EQUAL_COMB(c) ((u8)(c) + 192)

//we need to specify the value so we don't overlap, as lot's of token types
//represent the token char directly
typedef enum token_types_t{
	TOKEN_TYPE_STRING = 's',
	TOKEN_TYPE_NUMBER = 'n',
	TOKEN_TYPE_LITERAL = 'l',
	TOKEN_TYPE_BINARY_LITERAL = 'b',
	TOKEN_TYPE_POSSIBLY_UNARY = 'u',
	TOKEN_TYPE_OPERATOR_LR = 'x',
	TOKEN_TYPE_OPERATOR_L = 'y',
	TOKEN_TYPE_OPERATOR_R = 'z',
    TOKEN_TYPE_HASH = '#',
    TOKEN_TYPE_DOUBLE_HASH = 'd',
	TOKEN_TYPE_STAR = '*',	//this is separate from other operators
	TOKEN_TYPE_EOF = '\0',
    TOKEN_TYPE_NONE = 0xFF,
}token_types;
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
char* store_string(cunit* cu, char* str, char* str_end, ureg* str_pos);
void parse(cunit* cu, char* str);
void print_ast(cunit* cu);
void print_token(cunit* cu, token* t);
