#include "parser.h"
#include <stdio.h>
#include <memory.h>

void compilation_unit_init(compilation_unit* cu){
	dbuffer_init(&cu->string_store);
	dbuffer_init(&cu->string_ptrs);
	dbuffer_init(&cu->ast);
}
#define ASSERT_NEOF(c) do{if((c) == '\0'){printf("Unexpected EOF"); assert(false);}}while(0)

void compilation_unit_print_token(compilation_unit* cu, token* t){
	switch(t->type){
		case 'n':
		case 's': 
			fputs(t->str, stdout);
			return;
		case 'l': 
			putchar('\"'); 
			fputs(t->str, stdout); 
			putchar('\"'); 
			return;
		case 'b':
			putchar('\''); 
			fputs(t->str, stdout);
			putchar('\'');
			return;
	}
	if(t->type > 192){
		putchar(t->type);
		putchar('=');
	}
	else if(t->type > 128){
		putchar(t->type);
		putchar(t->type);
	}
	else{
		putchar(t->type);
	}
}

static inline int cmp_string_with_stored(char* str_start, char* str_end, char* stored){
	for(;;){
		if(*str_start != *stored){
			return *stored - *str_start;
		}
		if(str_start == str_end) return 0;
		str_start++;
		stored++;
	}
}
char* compilation_unit_store_string(compilation_unit* cu, char* str, char* str_end, ureg* str_pos){
	//we do this ahead so we don't have to worry about invalidating pointers
	dbuffer_make_small_space(&cu->string_ptrs, sizeof(ureg));
	ureg* sptrs_start = (ureg*)cu->string_ptrs.start;
	ureg* sptrs_end = (ureg*)cu->string_ptrs.head;
	ureg* pivot;
	int res;
	for(;;){
		pivot = sptrs_start + (sptrs_end - sptrs_start) / 2;	
		if(pivot == sptrs_start){
			ureg str_size = str_end - str;
			char* tgt = dbuffer_claim_space(&cu->string_store, str_size + 1); //+1 for \0
			memcpy(tgt, str, str_size);
			*(tgt + str_size) = '\0';
			memmove(sptrs_start+1, sptrs_start, cu->string_ptrs.head - (u8*)sptrs_start);
			*sptrs_start = (u8*)tgt - cu->string_store.start;
			*str_pos = *sptrs_start;
			return tgt;
		}
		res = cmp_string_with_stored(str, str_end, cu->string_store.start + *pivot);
		if(res < 0){
			sptrs_end = pivot;
		}
		else if(res > 0){
			sptrs_start = pivot +1;
		}
		else{
			*str_pos = *pivot;
			return cu->string_store.start + *pivot;
		}
	}
}

void compilation_unit_get_token(compilation_unit* cu, token* tok){
redo:;
	char curr = *cu->pos;
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
		tok->str = 
			compilation_unit_store_string(cu, str_start, str_end, &tok->str_rel);			
		tok->type = TOKEN_TYPE_STRING;
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
		tok->str = 
			compilation_unit_store_string(cu, str_start, str_end, &tok->str_rel);			
		tok->type = TOKEN_TYPE_NUMBER;
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
		tok->str = 
			compilation_unit_store_string(cu, str_start, str_end, &tok->str_rel);			
		tok->type = TOKEN_TYPE_LITERAL;
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
		tok->str = 
			compilation_unit_store_string(cu, str_start, str_end, &tok->str_rel);			
		tok->type = TOKEN_TYPE_BINARY_LITERAL;
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
			tok->type = TOKEN_TYPE_EOF;
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
	switch(curr){
		case '\n':
		case ' ':	
			cu->pos++;
			goto redo;
		case '+':
		case '-':
		case '|':
		case '=':
		case '&':{
			char nxt = *(cu->pos+1);
			if(nxt == curr){
				tok->type = TOKEN_TYPE_DOUBLE(curr);	
			}
			else if(nxt == '='){
				tok->type = TOKEN_TYPE_EQUAL_COMB(curr);	
			}
			else{
				tok->type = curr;
			}
			cu->pos ++;
			return;
		}
		case '*':
		case '/':
		case '%':
		case '\\':
		case '~':
		case '$':
		case '#':
		case ';':
			tok->type = curr; 
			cu->pos++;
			return;
		case '\0':
			tok->type = TOKEN_TYPE_EOF;
			cu->pos++;
			return;
		default:
			printf("unknown token: %c", curr);
			assert(false);
	}
}
