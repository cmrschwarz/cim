#pragma once
#define OPERATOR_DOUBLE(c)     ((u8)((u8)(c) + 128))
#define OPERATOR_EQUAL_COMB(c) ((u8)((u8)(c) + 192))
#define OPERATOR_WITHOUT_DOUBLE(c)     ((u8)((u8)(c) - 128))
#define OPERATOR_WITHOUT_EQUAL_COMB(c) ((u8)((u8)(c) - 192))
#define OPERATOR_IS_DOUBLE(c) ((c) > 128 && (c) < 192)
#define OPERATOR_IS_EQUAL_COMB(c) ((c) > 192)

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
    TOKEN_TYPE_EOF = '\0',
    TOKEN_TYPE_NONE = 0xFF,
}token_types;

enum OPS{
    OPS_UNYRY_PLUS='p',
    OPS_UNARY_MINUS='m',
    OPS_DEREF = 'd',
};

void print_token(cunit* cu, token* t);
void print_rel_str(cunit* cu, ureg str);
ureg store_string(cunit* cu, char* str, char* str_end);
void print_token(cunit* cu, token* t);
void get_token(cunit* cu, token* tok);