#include "parser.h"
#include <stdio.h>
#include <memory.h>
#include "ast.h"

void cunit_init(cunit* cu){
	dbuffer_init(&cu->string_store);
	dbuffer_init(&cu->string_ptrs);
	dbuffer_init(&cu->ast);
}
#define ASSERT_NEOF(c) do{if((c) == '\0'){printf("Unexpected EOF"); assert(false);}}while(0)
#define UNEXPECTED_TOKEN() do{                                         \
    printf("%s(%s): Unexpected token", __FILE__, __LINE__); exit(-1); \
}while(0)
static void cunit_print_rel_str(cunit* cu, ureg str){
   fputs((char*)(cu->string_store.start + str), stdout);
}
void cunit_print_token(cunit* cu, token* t){
	switch(t->type){
		case 'n':
		case 's': 
            cunit_print_rel_str(cu, t->str);
			return;
		case 'l': 
			putchar('\"'); 
            cunit_print_rel_str(cu, t->str);
			putchar('\"'); 
			return;
		case 'b':
			putchar('\''); 
            cunit_print_rel_str(cu, t->str);
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
char* cunit_store_string(cunit* cu, char* str, char* str_end, ureg* str_pos){
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
		res = cmp_string_with_stored(str, str_end, (char*)cu->string_store.start + *pivot);
		if(res < 0){
			sptrs_end = pivot;
		}
		else if(res > 0){
			sptrs_start = pivot +1;
		}
		else{
			*str_pos = *pivot;
			return (char*)cu->string_store.start + *pivot;
		}
	}
}

void cunit_get_token(cunit* cu, token* tok){
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
		cunit_store_string(cu, str_start, str_end, &tok->str);			
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
		cunit_store_string(cu, str_start, str_end, &tok->str);			
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
		cunit_store_string(cu, str_start, str_end, &tok->str);			
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
		cunit_store_string(cu, str_start, str_end, &tok->str);			
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
		case '~':
		case '|':
		case '=':
		case '&':{
			char nxt = *(cu->pos+1);
			if(nxt == curr){
				tok->type = TOKEN_TYPE_DOUBLE(curr);	
			    cu->pos += 2;
			}
			else if(nxt == '='){
				tok->type = TOKEN_TYPE_EQUAL_COMB(curr);	
			    cu->pos += 2;
			}
			else{
				tok->type = curr;
			    cu->pos ++;
			}
			return;
		}
		case '*':
		case '/':
		case '%':
		case '\\':
		case '$':
		case ';':
			tok->type = curr; 
			cu->pos++;
			return;
        case '#':
            if(*(cu->pos + 1) == curr){
				tok->type = TOKEN_TYPE_DOUBLE(curr);	
                cu->pos+=2;
			}
            else{
                tok->type = curr;
                cu->pos+=1;
            }
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
void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
}
void cunit_print_ast(cunit* cu){
    u8* astn = cu->ast.start;
    u8* end = cu->ast.head;
    ureg indent = 0;
    while(astn!=end){
        switch(*astn){
            case ASTNT_DECLARATION:{
                print_indent(indent);
                astn_declaration* d = (void*)astn;
                astn+= sizeof(*d);
                cunit_print_rel_str(cu, d->type);
                putchar(' ');
                cunit_print_rel_str(cu, d->name);
                if(d->assigning == false){
                    putchar(';');
                    putchar('\n');
                    break;
                }
                fputs(" = ", stdout);
                break;
            }
            case ASTNT_NUMBER:{
                astn_number* n = (void*)astn;
                astn+= sizeof(*n);
                cunit_print_rel_str(cu, n->number_str);
                putchar(';');
                putchar('\n');
                break;
            }               
            default:
                UNEXPECTED_TOKEN();
                return;
        } 
    }
}
static void cunit_parse_meta(cunit* cu, token* t1){
    
}

static int cunit_parse_expression(cunit* cu, char term, token* t1, token* t2){
    if(t2->type == term){
        //short expression optimization
        if(t1->type == TOKEN_TYPE_NUMBER){
            astn_number* n = dbuffer_claim_small_space(&cu->ast, sizeof(astn_number));
            n->astnt = ASTNT_NUMBER;
            n->number_str = t1->str;
        }  
        return 0;
    }
    token t;
    t.type = TOKEN_TYPE_NONE;
    while(t.type != term && t.type != TOKEN_TYPE_EOF){
       cunit_get_token(cu, &t); 
    }
    if(t.type == TOKEN_TYPE_EOF)return -1;
}
static int cunit_parse_normal_declaration(cunit* cu, token* t1, token* t2){
    astn_declaration* d =
         dbuffer_claim_small_space(&cu->ast, sizeof(astn_declaration));
    d->astnt = ASTNT_DECLARATION;
    d->type = t1->str;
    d->name = t2->str;
    token nt1;
    cunit_get_token(cu, &nt1);
    if(nt1.type == ';'){
        d->assigning = false;
    }
    else if(nt1.type == '='){
        d->assigning = true;
        cunit_get_token(cu, &nt1);
        token nt2;
        cunit_get_token(cu, &nt2);
        return cunit_parse_expression(cu, ';', &nt1, &nt2);
    }
    else{
        UNEXPECTED_TOKEN();    
    }
    return 0;
    printf("parsed declaration\n"); 
}
static int cunit_parse_next(cunit* cu){
    token t1;
    token t2;
    cunit_get_token(cu, &t1);
    switch (t1.type){
        case TOKEN_TYPE_STRING:
            cunit_get_token(cu, &t2);
            if (t2.type == TOKEN_TYPE_STRING) 
                return cunit_parse_normal_declaration(cu, &t1, &t2);
            else return cunit_parse_expression(cu, ';', &t1, &t2);
        case TOKEN_TYPE_NUMBER:
            cunit_get_token(cu, &t2);
            return cunit_parse_expression(cu, ';', &t1, &t2);
        case '#':
        case TOKEN_TYPE_DOUBLE('#'):
            cunit_parse_meta(cu, &t1);
            break;
        case TOKEN_TYPE_EOF:
            return -1;
        default:
            printf("parse_next: unexpected token\n");
            return -1;
    }
    return 0;
}
void cunit_parse(cunit* cu, char* str){
    cu->str = str;
    /*
    token t;
	cunit_get_token(cu, &t);
	while(t.type != TOKEN_TYPE_EOF){
		cunit_print_token(cu, &t);
		printf(" (%i)", t.rel_str);
		putchar('\n');
		cunit_get_token(cu, &t);
	}	
    */
    // actual parsing
    while(cunit_parse_next(cu)!=-1); 
}
