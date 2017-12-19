#include "types.h"
#include "dbuffer.h"
#define TOKEN_TYPE_DOUBLE(c)((c) + 128)
#define TOKEN_TYPE_EQUAL_COMB(c) ((c) + 192)
typedef enum token_types_t{
	TOKEN_TYPE_STRING = 's',
	TOKEN_TYPE_NUMBER = 'n',
	TOKEN_TYPE_LITERAL = 'l',
	TOKEN_TYPE_BINARY_LITERAL = 'b',
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
}cunit;


void cunit_init(cunit* cu);
void cunit_get_token(cunit* cu, token* t);
char* cunit_store_string(cunit* cu, char* str, char* str_end, ureg* str_pos);
void cunit_parse(cunit* cu, char* str);
void cunit_print_ast(cunit* cu);
void cunit_print_token(cunit* cu, token* t);
