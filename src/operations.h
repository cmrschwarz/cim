#pragma once
typedef enum operation_e{
    OP_SUBTRACT_ASSIGN = TOKEN_MINUS_EQUALS,
    OP_ADD_ASSIGN = TOKEN_PLUS_EQUALS,
    OP_MULTIPLY_ASSIGN = TOKEN_STAR_EQUALS,
    OP_ASSIGN = TOKEN_EQUALS,
    OP_EQUALS = TOKEN_DOUBLE_EQUALS,
    OP_LOGICAL_NOT = TOKEN_EXCLAMATION_MARK,
    OP_NOT_EQUAL = TOKEN_EXCLAMATION_MARK_EQUALS,
    OP_MODULO = TOKEN_PERCENT,
    OP_MODULO_ASSIGN = TOKEN_PERCENT_EQUALS,
    OP_DIVIDE = TOKEN_SLASH,
    OP_DIVIDE_ASSIGN = TOKEN_SLASH_EQUALS,
    OP_LESS_THAN = TOKEN_LESS_THAN,
    OP_LESS_THAN_OR_EQUAL = TOKEN_LESS_THAN_EQUALS,
    OP_LSHIFT = TOKEN_DOUBLE_LESS_THAN,
    OP_LSHIFT_ASSIGN = TOKEN_DOUBLE_LESS_THAN_EQUALS,
    OP_GREATER_THAN = TOKEN_GREATER_THAN,
    OP_GREATER_THAN_OR_EQUAL = TOKEN_GREATER_THAN_EQUALS,
    OP_RSHIFT = TOKEN_DOUBLE_GREATER_THAN,
    OP_RSHIFT_ASSIGN = TOKEN_DOUBLE_GREATER_THAN_EQUALS,
    OP_BITWISE_AND = TOKEN_AND,
    OP_LOGICAL_AND = TOKEN_DOUBLE_AND,
	OP_BITWISE_AND_ASSIGN = TOKEN_AND_EQUALS,
	OP_BITWISE_OR = TOKEN_PIPE,
	OP_LOGICAL_OR = TOKEN_DOUBLE_PIPE,
	OP_BITWISE_OR_ASSIGN = TOKEN_PIPE_EQUALS,
    OP_BITWISE_XOR = TOKEN_CARET,
    OP_LOGICAL_XOR = TOKEN_DOUBLE_CARET,
    OP_BITWISE_XOR_ASSIGN = TOKEN_CARET_EQUALS,
    OP_BITWISE_NOT = TOKEN_TILDE,
	OP_BITWISE_NOT_ASSIGN=TOKEN_TILDE_EQUALS,

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
    OP_ADDRESS_OF,
	OP_MULTIPLY,

    OP_TEMP_PAREN_OPEN,
    OP_ACCESS_MEMBER,
    OP_DEREFERENCE_ACCESS_MEMBER,
};