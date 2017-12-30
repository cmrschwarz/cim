#include "parser.h"
#include "tokenizer.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#define ASSERT_NEOF(c) do{if((c) == '\0'){printf("Unexpected EOF"); assert(false);}}while(0)

void print_rel_str(cunit* cu, ureg str){
   fputs((char*)(cu->string_store.start + str), stdout);
}

static inline int cmp_string_with_stored(char* str_start, const char* str_end, char* stored){
	for(;;){
        if(str_start == str_end) return 0;
		if(*str_start != *stored){
			return  *str_start - *stored;
		}
		str_start++;
		stored++;
	}
}
ureg store_string(cunit* cu, char* str, char* str_end){
	//we do this ahead so we don't have to worry about invalidating pointers
	dbuffer_make_small_space(&cu->string_ptrs, sizeof(ureg));
	ureg* sptrs_start = (ureg*)cu->string_ptrs.start;
	ureg* sptrs_end = (ureg*)cu->string_ptrs.head;
	ureg* pivot;
	int res;
	for(;;){
		pivot = sptrs_start + (sptrs_end - sptrs_start) / 2;
		if(pivot == sptrs_start){
            if(pivot != (ureg*)cu->string_ptrs.head){
                if(cmp_string_with_stored(str, str_end, (char*)cu->string_store.start + *pivot) == 0){
                    return *pivot;
                }
            }
            ureg str_size = str_end - str;
            char* tgt = dbuffer_claim_space(&cu->string_store, str_size + 1); //+1 for \0
            memcpy(tgt, str, str_size);
            *(tgt + str_size) = '\0';
            ureg pos = (u8*)tgt - cu->string_store.start;
            dbuffer_insert_at(&cu->string_ptrs, &pos, sptrs_start, sizeof(ureg));
            return pos;
        }
        res = cmp_string_with_stored(str, str_end, (char*)cu->string_store.start + *pivot);
		if(res < 0){
			sptrs_end = pivot;
		}
		else if(res > 0){
			sptrs_start = pivot +1;
		}
		else{
			return *pivot;
		}
	}
}

void get_token(cunit* cu, token* tok){
redo:;
	char curr = *(cu->pos);
    while(curr == ' ' || curr == '\n'){
		cu->pos++;
        curr = *(cu->pos);
	}
	if((curr>= 'a' && curr <= 'z') || (curr >= 'A' && curr <= 'Z') || curr == '_'){
		char* str_start = cu->pos;
		char* str_end = str_start + 1;
		char c = *str_end;
		while((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			  (c >= '0' && c <= '9') || (c == '_'))
		{
			ASSERT_NEOF(c);
			str_end++;
			c = *str_end;
		}
        tok->str = store_string(cu, str_start, str_end);
		tok->type = TOKEN_STRING;
		cu->pos = str_end;
		return;
	}
	if(curr>= '0' && curr <= '9'){
		char* str_start = cu->pos;
		char* str_end = str_start + 1;
		char c = *str_end;
		while(c >= '0' && c <= '9'){
			ASSERT_NEOF(c);
			str_end++;
			c = *str_end;
		}
        tok->str = store_string(cu, str_start, str_end);
		tok->type = TOKEN_NUMBER;
		cu->pos = str_end;
		return;
	}
	if(curr == '\"'){
		char* str_start = cu->pos;
		char* str_end = str_start + 1;
		while(*str_end != '\"'){
			ASSERT_NEOF(*str_end);
			str_end++;
			if(*str_end == '\\')str_end ++;
		}
        tok->str = store_string(cu, str_start, str_end);
		tok->type = TOKEN_LITERAL;
		cu->pos = str_end;
		return;
	}
	if(curr == '\''){
		char* str_start = cu->pos;
		char* str_end = str_start + 1;
		while(*str_end != '\''){
			ASSERT_NEOF(*str_end);
			str_end++;
			if(*str_end == '\\'){
				str_end ++;
				ASSERT_NEOF(*str_end);
			}
		}
        tok->str = store_string(cu, str_start, str_end);
		tok->type = TOKEN_BINARY_LITERAL;
		cu->pos = str_end;
		return;
	}
	if(curr == '/' && *(cu->pos + 1) == '/'){
		char* cmt_end = cu->pos + 1;
		while(*cmt_end != '\n' && *cmt_end != '\0'){
			cmt_end++;
		}
		cu->pos = cmt_end;
		if(*cmt_end == '\0'){
			tok->type = TOKEN_EOF;
			return;
		}
		goto redo;
	}
	if(curr == '/' && *(cu->pos + 1) == '*'){
		char* cmt_end = cu->pos + 1;
		while(!(*cmt_end == '*' && *(cmt_end+1) == '/')){
			ASSERT_NEOF(*cmt_end);
			cmt_end++;
		}
		cu->pos = cmt_end;
		goto redo;
	}
    cu->pos++;
	switch(curr){
		case '$': tok->type = TOKEN_DOLLAR;return;
        case '(': tok->type = TOKEN_PAREN_OPEN;return;
        case ')': tok->type = TOKEN_PAREN_CLOSE;return;
        case '{': tok->type = TOKEN_BRACE_OPEN;return;
        case '}': tok->type = TOKEN_BRACE_CLOSE;return;
        case ']': tok->type = TOKEN_BRACKET_OPEN;return;
        case '[': tok->type = TOKEN_BRACKET_CLOSE;return;
        case ',': tok->type = TOKEN_COMMA;return;
		case ';': tok->type = TOKEN_SEMICOLON; return;
        case '.': tok->type = TOKEN_DOT; return;
        case '*': {
            curr = *cu->pos;
            if(curr == '=') {
                cu->pos++;
                tok->type = TOKEN_STAR_EQUALS;
            }
            else{
                 tok->type = TOKEN_STAR;
            }
        } return;
        case '+': {
            curr = *cu->pos;
            if(curr == '+') {
                cu->pos++;
                tok->type = TOKEN_DOUBLE_PLUS;
            }
            else if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_PLUS_EQUALS;
            }
            else{
                tok->type = TOKEN_PLUS;
            }
        } return;
	    case '-': {
            curr = *cu->pos;
            if(curr == '-') {
                cu->pos++;
                tok->type = TOKEN_DOUBLE_MINUS;
            }
            else if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_MINUS_EQUALS;
            }
            else if(curr == '>'){
                cu->pos++;
                tok->type = TOKEN_ARROW;
            }
            else{
                tok->type = TOKEN_MINUS;
            }
        } return;
        case '!': {
            curr = *cu->pos;
            if(curr == '=') {
                cu->pos++;
                tok->type = TOKEN_EXCLAMATION_MARK_EQUALS;
            }
            else{
                tok->type = TOKEN_EXCLAMATION_MARK;
            }
        } return;
        case '|': {
            curr = *cu->pos;
            if(curr == '|') {
                cu->pos++;
                tok->type = TOKEN_DOUBLE_PIPE;
            }
            else if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_PIPE_EQUALS;
            }
            else{
                tok->type = TOKEN_PIPE;
            }
        } return;
		case '&': {
            curr = *cu->pos;
            if(curr == '&') {
                cu->pos++;
                tok->type = TOKEN_DOUBLE_AND;
            }
            else if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_AND_EQUALS;
            }
            else{
                tok->type = TOKEN_AND;
            }
        } return;
        case '~': {
            curr = *cu->pos;
            if(curr == '=') {
                cu->pos++;
                tok->type = TOKEN_TILDE_EQUALS;
            }
            else{
                 tok->type = TOKEN_TILDE;
            }
        } return;
       case '=': {
            curr = *cu->pos;
            if(curr == '=') {
                cu->pos++;
                tok->type = TOKEN_DOUBLE_EQUALS;
            }
            else{
                 tok->type = TOKEN_EQUALS;
            }
        } return;
        case '/': {
            curr = *cu->pos;
            if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_SLASH_EQUALS;
            }
            else{
                tok->type = TOKEN_SLASH;
            }
        } return;
		case '%': {
            curr = *cu->pos;
            if(curr == '='){
                cu->pos++;
                tok->type = TOKEN_PERCENT_EQUALS;
            }
            else{
                tok->type = TOKEN_PERCENT;
            }
        } return;
        case '#': {
            curr = *cu->pos;
            if(curr == '#'){
                cu->pos++;
                tok->type = TOKEN_DOUBLE_HASH;
            }
            else{
                tok->type = TOKEN_HASH;
            }
        } return;
        case '<': {
            curr = *cu->pos;
            if(curr == '<'){
                cu->pos++;
                curr = *cu->pos;
                if(curr == '='){
                    cu->pos++;
                    tok->type = TOKEN_DOUBLE_LESS_THAN_EQUALS;
                }
                else{
                    tok->type = TOKEN_DOUBLE_LESS_THAN;
                }
            }
            else{
                tok->type = TOKEN_LESS_THAN;
            }
        } return;
        case '>': {
            curr = *cu->pos;
            if(curr == '>'){
                cu->pos++;
                curr = *cu->pos;
                if(curr == '='){
                    cu->pos++;
                    tok->type = TOKEN_DOUBLE_GREATER_THAN_EQUALS;
                }
                else{
                    tok->type = TOKEN_DOUBLE_GREATER_THAN;
                }
            }
            else{
                tok->type = TOKEN_GREATER_THAN;
            }
        } return;
		case '\0':{
            tok->type = TOKEN_EOF;
        }return;
		default:{
            printf("unknown token: %c", curr);
        }assert(false);
	}
}