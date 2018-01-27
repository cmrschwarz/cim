#pragma once
typedef struct{
	token_type type;
    char* str;
    ureg column;
    ureg line;
}token;
#define TOKEN_BUFFER_SIZE 4
typedef struct {
	ureg line;
    ureg column;
	char* filename;
    FILE* file;
    char* curr;
    dbuffer file_buffer;
	token token_buffer[TOKEN_BUFFER_SIZE];
	token* token_start;
	token* token_end;
}tokenizer;