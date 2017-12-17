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
}token_types;
typedef struct token_t{
	u8 type;
	char* str;
	ureg str_rel;
}token;

char* read_in_file(char* filename);

typedef struct compilation_unit_t{
	//later on, this will read the file live, but this is easier for now
	char* str;
	char* pos;
	token current_token;
	token next_token;
	
	dbuffer string_store;
	dbuffer string_ptrs;
	//eventually, one ast per function?
	dbuffer ast;
}compilation_unit;


void compilation_unit_init(compilation_unit* c);
void compilation_unit_consume_token(compilation_unit* c);
char* compilation_unit_store_string(compilation_unit* cu, char* str, char* str_end, ureg* str_pos);
void compilation_unit_parse(compilation_unit* c, char* str);
void compilation_unit_print_ast(compilation_unit* c);
void compilation_unit_print_token(compilation_unit* cu, token* t);
