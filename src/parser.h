#pragma once
#include "compiler.h"
#include "tokenizer.h"
enum OP{
    OP_SUBTRACT_ASSIGN = TOKEN_MINUS_EQUALS,
    OP_ADD_ASSIGN = TOKEN_PLUS_EQUALS,
    OP_MULTIPLY_ASSIGN = TOKEN_STAR_EQUALS,
    OP_ASSIGN = TOKEN_EQUALS,
    OP_EQUALS = TOKEN_DOUBLE_EQUALS,
    OP_NOT_EQUAL = TOKEN_EXCLAMATION_MARK_EQUALS,
    OP_MODULO = TOKEN_PERCENT,
    OP_MODULO_ASSIGN = TOKEN_PERCENT_EQUALS,
    OP_DIVIDE = TOKEN_SLASH,
    OP_DIVIDE_ASSIGN = TOKEN_SLASH_EQUALS,
    OP_LESS_THAN = TOKEN_LESS_THAN,
    OP_LSHIFT = TOKEN_DOUBLE_LESS_THAN,
    OP_LSHIFT_ASSIGN = TOKEN_DOUBLE_LESS_THAN_EQUALS,
    OP_GREATER_THAN = TOKEN_GREATER_THAN,
    OP_RSHIFT = TOKEN_DOUBLE_GREATER_THAN,
    OP_RSHIFT_ASSIGN = TOKEN_DOUBLE_GREATER_THAN_EQUALS,
    OP_NONE,
    OP_ADD,
	OP_UNARY_PLUS,
    OP_PREINCREMENT,
	OP_POSTINCREMENT,
	OP_UNARY_MINUS,
    OP_SUBTRACT,
    OP_PREDECREMENT,
	OP_POSTDECREMENT,
    OP_DEREFERENCE,
	OP_MULTIPLY,
	OP_BITWISE_AND,
	OP_LOGICAL_AND,
	OP_BITWISE_AND_ASSIGN,
	OP_BITWISE_OR,
	OP_LOGICAL_OR,
	OP_BITWISE_OR_ASSIGN,
	OP_LOGICAL_NOT,
    OP_BITWISE_NOT,
	OP_BITWISE_NOT_ASSIGN,
    OP_BITWISE_XOR,
    OP_BITWISE_XOR_ASSIGN,
    OP_TEMP_PAREN_OPEN,
    OP_ACCESS_MEMBER,
    OP_DEREFERENCE_ACCESS_MEMBER,

};



char* read_in_file(char* filename);


void init(cunit* cu);

void parse(cunit* cu, char* str);
void print_ast(cunit* cu);
