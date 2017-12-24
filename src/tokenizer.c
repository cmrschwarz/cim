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
void print_token(cunit* cu, token* t){
    u8 op = TO_CHAR(t->str);
	switch(t->type){
		case TOKEN_TYPE_NUMBER:
		case TOKEN_TYPE_STRING:
            print_rel_str(cu, t->str);
			return;
		case TOKEN_TYPE_LITERAL:
			putchar('\"');
            print_rel_str(cu, t->str);
			putchar('\"');
			return;
		case TOKEN_TYPE_BINARY_LITERAL:
			putchar('\'');
            print_rel_str(cu, t->str);
			putchar('\'');
			return;
        case TOKEN_TYPE_POSSIBLY_UNARY:{
            switch(TO_CHAR(t->str)) {
                case OPS_UNYRY_PLUS:putchar('+');return;
                case OPS_UNARY_MINUS:putchar('-');return;
                case OPS_DEREF:putchar('*');return;
                default: printf("unknown unary op\n");exit(-1);
            }
        }
		case TOKEN_TYPE_OPERATOR_L:
        case TOKEN_TYPE_OPERATOR_R:{
            if(OPERATOR_IS_DOUBLE(op)){
                putchar(OPERATOR_WITHOUT_DOUBLE(op));
                putchar(OPERATOR_WITHOUT_DOUBLE(op));
            }
            else if (OPERATOR_IS_EQUAL_COMB(op)){
                putchar(OPERATOR_WITHOUT_EQUAL_COMB(op));
                putchar('=');
            }
            else{
                putchar(op);
            }
        }return;
        case TOKEN_TYPE_OPERATOR_LR:
        {
            putchar(op);
        }return;
        default:
            printf("Unknown token\n");exit(-1);
	}
}

static inline int cmp_string_with_stored(char* str_start, char* str_end, char* stored){
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
            printf("String cache miss for %s [%llu]\n", tgt, pos);
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
        tok->str = store_string(cu, str_start, str_end);
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
        tok->str = store_string(cu, str_start, str_end);
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
        tok->str = store_string(cu, str_start, str_end);
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
		case ' ':{
            cu->pos++;
        }goto redo;

        case '\n':
		case '\\':
		case '$':
        case '(':
        case ')':
        case '{':
        case '}':
        case ']':
        case '[':
        case ',':
		case ';':{
            tok->type = curr;
            cu->pos++;
        }return;

        case '*':
		case '+':
		case '-':{
			char nxt = *(cu->pos+1);
			if(nxt == curr){
                tok->type = TOKEN_TYPE_OPERATOR_L_OR_R;
				tok->str = OPERATOR_DOUBLE(curr);
			    cu->pos += 2;
			}
			else if(nxt == '='){
                tok->type = TOKEN_TYPE_OPERATOR_LR;
				tok->str = OPERATOR_EQUAL_COMB(curr);
			    cu->pos += 2;
			}
			else {
                tok->type = TOKEN_TYPE_POSSIBLY_UNARY;
                tok->str = curr;
			    cu->pos ++;
			}
		}return;

		case '|':
		case '&':{
            tok->type = TOKEN_TYPE_OPERATOR_LR;
			char nxt = *(cu->pos+1);
			if(nxt == curr){
				tok->str = OPERATOR_DOUBLE(curr);
			    cu->pos += 2;
			}
			else if(nxt == '='){
				tok->str = OPERATOR_EQUAL_COMB(curr);
			    cu->pos += 2;
			}
			else{
                tok->str = curr;
			    cu->pos ++;
			}
		}return;

        case '~':{
			char nxt = *(cu->pos+1);
			if(nxt == '='){
                tok->type = TOKEN_TYPE_OPERATOR_LR;
				tok->str = OPERATOR_EQUAL_COMB(curr);
			    cu->pos += 2;
			}
			else{
                tok->type = TOKEN_TYPE_OPERATOR_R;
				tok->str = curr;
			    cu->pos ++;
			}
		}return;


        case '=':{
			char nxt = *(cu->pos+1);
			if(nxt == '='){
                tok->type = TOKEN_TYPE_OPERATOR_LR;
				tok->str = OPERATOR_EQUAL_COMB(curr);
			    cu->pos += 2;
			}
			else{
                tok->type = TO_U8(curr);
			    cu->pos ++;
			}
		}return;

        case '/':
		case '%':
		{
            tok->type = TOKEN_TYPE_OPERATOR_LR;
			char nxt = *(cu->pos+1);
			if(nxt == '='){
				tok->str = OPERATOR_EQUAL_COMB(curr);
			    cu->pos += 2;
			}
			else{
				tok->str = TO_U8(curr);
			    cu->pos ++;
			}

		}return;

        case '#':{
            if(*(cu->pos + 1) == curr){
				tok->type = TOKEN_TYPE_DOUBLE_HASH;
                cu->pos+=2;
			}
            else{
                tok->type = TOKEN_TYPE_HASH;
                cu->pos+=1;
            }
        } return;

		case '\0':{
            tok->type = TOKEN_TYPE_EOF;
			cu->pos++;
        }return;

		default:{
            printf("unknown token: %c", curr);
        }assert(false);
	}
}