#include "parser.h"
#include "tokenizer.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "error.h"
#include "compiler.h"

#define ASSERT_NEOF(c) do{if((c) == '\0'){CIM_ERROR("Unexpected EOF");}}while(0)
static void populate_file_buffer(cunit* cu){
    ureg s = fread(cu->tknzr.curr,
                   1,cu->tknzr.file_buffer.end - (u8*)cu->tknzr.curr,
                   cu->tknzr.file);
    cu->tknzr.file_buffer.head =  (u8*)cu->tknzr.curr + s;
}
void tokenizer_open_file(cunit* cu, char* filename) {
    if(cu->tknzr.file != NULL){
        fclose(cu->tknzr.file);
    }
    cu->tknzr.file = fopen(filename, "r");
    if(cu->tknzr.file == NULL){
        printf("Failed to open file %s", filename);
        exit(-1);
    }
    cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
    cu->tknzr.filename = filename;
    cu->tknzr.line_number = 0;
    populate_file_buffer(cu);
}
void tokenizer_close_file(cunit* cu){
    fclose(cu->tknzr.file);
    cu->tknzr.filename = NULL;
    cu->tknzr.file = NULL;
}
static inline void unread_char(cunit* cu){
     cu->tknzr.curr--;
}
static inline char peek_char(cunit* cu){
    if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head){
        return *cu->tknzr.curr;
    }
    else{
        cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
        populate_file_buffer(cu);
        if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head){
            return *cu->tknzr.curr;
        }
        else{
            return '\0';
        }
    }
}
static inline void void_peek(cunit* cu){
    cu->tknzr.curr+=1;
}
static inline char peek_string_char(cunit* cu, char** str_start){
    if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head) {
        return *cu->tknzr.curr;
    }
    else{
        ureg size = cu->tknzr.curr - *str_start;
        if((u8*)*str_start != cu->tknzr.file_buffer.start) {
            memmove(cu->tknzr.file_buffer.start, *str_start, size);
            cu->tknzr.curr = (char *) (cu->tknzr.file_buffer.start + size);
            populate_file_buffer(cu);
            if ((u8 *) cu->tknzr.curr != cu->tknzr.file_buffer.head) {
                *str_start = (char *) cu->tknzr.file_buffer.start;
                return *cu->tknzr.curr;
            }
        }
        //not enough space
        dbuffer_grow(&cu->tknzr.file_buffer);
        cu->tknzr.curr = (char*)(cu->tknzr.file_buffer.start + size);
        populate_file_buffer(cu);
        if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head) {
            *str_start = (char*)cu->tknzr.file_buffer.start;
            return *cu->tknzr.curr;
        }
        else{
            return '\0';
        }
    };
}
void display_string_store(cunit* cu){
    char** t = (void*)cu->string_ptrs.start;
    while(t != (void*)cu->string_ptrs.head){
        printf("%s\n",*t);
        t++;
    }
}
static inline int cmp_unended_string_with_stored(char* str_start, const char* str_end, char* stored){
	for(;;){
        if(str_start == str_end){
            if(*stored == '\0')return 0;
            return -*stored;
        }
		if(*str_start != *stored){
			return  *str_start - *stored;
		}
		str_start++;
		stored++;
	}
}
void add_keyword(cunit* cu, const char* str){
    char** sptrs_start = (char**)cu->string_ptrs.start;
	char** sptrs_end = (char**)cu->string_ptrs.head;
	char** pivot;
    int res;
    for(;;) {
        if(sptrs_end == sptrs_start) {
            if (sptrs_start == (char**)cu->string_ptrs.head){
                *(const char**)dbuffer_claim_small_space(&cu->string_ptrs,
                                                   sizeof(char**)) = str;
            }
            else{
                if(strcmp(str, *sptrs_end) == 0) CIM_ERROR("keyword already present");
                 dbuffer_insert_at(&cu->string_ptrs, &str, sptrs_end, sizeof(char*));
            }
            return;
        }
        pivot = sptrs_start + (sptrs_end - sptrs_start) / 2;
        res = strcmp(str, *pivot);
        if(res < 0){
			sptrs_end = pivot;
		}
		else if(res > 0){
			sptrs_start = pivot +1;
		}
		else{
			CIM_ERROR("keyword already present");
		}
    }
}
char* store_string(cunit* cu, char* str, char* str_end){
	//we do this ahead so we don't have to worry about invalidating pointers
	dbuffer_make_small_space(&cu->string_ptrs, sizeof(char*));
	char** sptrs_start = (char**)cu->string_ptrs.start;
	char** sptrs_end = (char**)cu->string_ptrs.head;
	char** pivot;
	int res;
	for(;;){
		if(sptrs_end == sptrs_start){
            if(sptrs_start != (char**)cu->string_ptrs.head &&
               cmp_unended_string_with_stored(str, str_end, *sptrs_end) == 0)
            {
                return *sptrs_end;
            }
            ureg str_size = str_end - str;
            char* tgt = sbuffer_append(&cu->data_store, str_size + 1); //+1 for \0
            memcpy(tgt, str, str_size);
            *(tgt + str_size) = '\0';
            dbuffer_insert_at(&cu->string_ptrs, &tgt, sptrs_end, sizeof(char*));
            return tgt;
        }
        pivot = sptrs_start + (sptrs_end - sptrs_start) / 2;
        res = cmp_unended_string_with_stored(str, str_end, *pivot);
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

void consume_new_token(cunit* cu, token* tok){
	char curr = peek_char(cu);
    while(true) {
        if (curr == ' ') {
            void_peek(cu);
            curr = peek_char(cu);
        }
        else if (curr == '\n') {
            void_peek(cu);
            curr = peek_char(cu);
            cu->tknzr.line_number++;
        }
        else {
            break;
        }
    }
	if((curr>= 'a' && curr <= 'z') || (curr >= 'A' && curr <= 'Z') || curr == '_'){
		char* str_start = cu->tknzr.curr;
        do{
            void_peek(cu);
            curr = peek_string_char(cu, &str_start);
			ASSERT_NEOF(curr);
		}while((curr >= 'a' && curr <= 'z') ||
               (curr >= 'A' && curr <= 'Z') ||
			   (curr >= '0' && curr <= '9') ||
               (curr == '_'));
        tok->str = store_string(cu, str_start, cu->tknzr.curr);
		tok->type = TOKEN_STRING;
		return;
	}
	if(curr>= '0' && curr <= '9'){
		char* str_start = cu->tknzr.curr;
        do{
            void_peek(cu);
            curr = peek_string_char(cu, &str_start);
			ASSERT_NEOF(curr);
		}while(curr >= '0' && curr <= '9');
        tok->str = store_string(cu, str_start, cu->tknzr.curr);
		tok->type = TOKEN_NUMBER;
		return;
	}
	if(curr == '\"'){
		char* str_start = cu->tknzr.curr;
        do{
            void_peek(cu);
            curr = peek_string_char(cu, &str_start);
			ASSERT_NEOF(curr);
			if(curr == '\\'){
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
            }
            if(curr == '\n'){
                cu->tknzr.line_number++;
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
            }
		}while(curr != '\"');
        tok->str = store_string(cu, str_start, cu->tknzr.curr);
		tok->type = TOKEN_LITERAL;
		return;
	}
	if(curr == '\''){
		char* str_start = cu->tknzr.curr;
        do{
            void_peek(cu);
            curr = peek_string_char(cu, &str_start);
			ASSERT_NEOF(curr);
			if(curr == '\\'){
				void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
            }
            if(curr == '\n'){
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
                ASSERT_NEOF(curr);
            }
		}while(curr != '\'');
        tok->str = store_string(cu, str_start, cu->tknzr.curr);
		tok->type = TOKEN_BINARY_LITERAL;
        return;
	}
    void_peek(cu);
	switch(curr){
		case '$': tok->type = TOKEN_DOLLAR;return;
        case '(': tok->type = TOKEN_PAREN_OPEN;return;
        case ')': tok->type = TOKEN_PAREN_CLOSE;return;
        case '{': tok->type = TOKEN_BRACE_OPEN;return;
        case '}': tok->type = TOKEN_BRACE_CLOSE;return;
        case '[': tok->type = TOKEN_BRACKET_OPEN;return;
        case ']': tok->type = TOKEN_BRACKET_CLOSE;return;
        case ',': tok->type = TOKEN_COMMA;return;
		case ';': tok->type = TOKEN_SEMICOLON; return;
        case '.': tok->type = TOKEN_DOT; return;
        case ':': tok->type = TOKEN_COLON; return;
        case '*': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_STAR_EQUALS;
            }
            else{
                 tok->type = TOKEN_STAR;
            }
        } return;
        case '+': {
            char peek = peek_char(cu);
            if(peek == '+') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_PLUS;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PLUS_EQUALS;
            }
            else{
                tok->type = TOKEN_PLUS;
            }
        } return;
	    case '-': {
            char peek = peek_char(cu);
            if(peek == '-') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_MINUS;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_MINUS_EQUALS;
            }
            else if(peek == '>'){
                void_peek(cu);
                tok->type = TOKEN_ARROW;
            }
            else{
                tok->type = TOKEN_MINUS;
            }
        } return;
        case '!': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_EXCLAMATION_MARK_EQUALS;
            }
            else{
                tok->type = TOKEN_EXCLAMATION_MARK;
            }
        } return;
        case '|': {
            char peek = peek_char(cu);
            if(peek == '|') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_PIPE;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PIPE_EQUALS;
            }
            else{
                tok->type = TOKEN_PIPE;
            }
        } return;
		case '&': {
            char peek = peek_char(cu);
            if(peek == '&') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_AND;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_AND_EQUALS;
            }
            else{
                tok->type = TOKEN_AND;
            }
        } return;
        case '^': {
            char peek = peek_char(cu);
            if(peek == '^') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_CARET;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_CARET_EQUALS;
            }
            else{
                tok->type = TOKEN_CARET;
            }
        } return;
        case '~': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_TILDE_EQUALS;
            }
            else{
                 tok->type = TOKEN_TILDE;
            }
        } return;
       case '=': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_EQUALS;
            }
            else{
                 tok->type = TOKEN_EQUALS;
            }
        } return;
        case '/': {
            char peek = peek_char(cu);
            if(peek == '/'){
                do{
                    void_peek(cu);
                    curr = peek_char(cu);
                    if(curr == '\\'){
                        void_peek(cu);
                        curr = peek_char(cu);
                        ASSERT_NEOF(curr);
                        void_peek(cu);
                        curr = peek_char(cu);
                        ASSERT_NEOF(curr);
                    }
                    if(curr == '\n'){
                        cu->tknzr.line_number++;
                        void_peek(cu);
                        curr = peek_char(cu);
                        ASSERT_NEOF(curr);
                    }
                }while(curr != '\n' && curr != '\0');
                ASSERT_NEOF(curr);
                consume_new_token(cu, tok);
                return;
            }
            if(peek == '*'){
                do{
                    do{
                        void_peek(cu);
                        curr = peek_char(cu);
                        if(curr == '\\'){
                            void_peek(cu);
                            curr = peek_char(cu);
                            ASSERT_NEOF(curr);
                            void_peek(cu);
                            curr = peek_char(cu);
                        }
                        if(curr == '\n'){
                            cu->tknzr.line_number++;
                            void_peek(cu);
                            curr = peek_char(cu);
                        }
                    }while(curr != '*' && curr != '\0');
                    ASSERT_NEOF(curr);
                    void_peek(cu);
                    peek = peek_char(cu);
                }while(peek != '/');
                void_peek(cu);
                consume_new_token(cu, tok);
                return;
            }
            if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_SLASH_EQUALS;
            }
            else{
                tok->type = TOKEN_SLASH;
            }
        } return;
		case '%': {
            char peek = peek_char(cu);
            if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PERCENT_EQUALS;
            }
            else{
                tok->type = TOKEN_PERCENT;
            }
        } return;
        case '#': {
            char peek = peek_char(cu);
            if(peek == '#'){
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_HASH;
            }
            else{
                tok->type = TOKEN_HASH;
            }
        } return;
        case '<': {
            char peek = peek_char(cu);
            if(peek == '<'){
                void_peek(cu);
                peek = peek_char(cu);
                if(peek == '='){
                    void_peek(cu);
                    tok->type = TOKEN_DOUBLE_LESS_THAN_EQUALS;
                }
                else{
                    tok->type = TOKEN_DOUBLE_LESS_THAN;
                }
            }
            else if(peek == '='){
                tok->type = TOKEN_LESS_THAN_EQUALS;
            }
            else{
                tok->type = TOKEN_LESS_THAN;
            }
        } return;
        case '>': {
            char peek = peek_char(cu);
            if(peek == '>'){
                void_peek(cu);
                peek = peek_char(cu);
                if(peek == '='){
                    void_peek(cu);
                    tok->type = TOKEN_DOUBLE_GREATER_THAN_EQUALS;
                }
                else{
                    tok->type = TOKEN_DOUBLE_GREATER_THAN;
                }
            }
            else if(peek == '='){
                tok->type = TOKEN_GREATER_THAN_EQUALS;
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

/*
ureg get_curr_src_line(cunit* cu){
    return cu->tknzr.line_number;
}

ureg get_curr_src_column(cunit* cu){
    char* s = cu->tknzr.pos;
    ureg column = 0;
    while(*s!= '\n' && s != cu->tknzr.str){
        s--;
        column++;
    }
    return column;
}
char* get_curr_src_file_name(cunit* cu){
    return cu->tknzr.filename;
}
static inline void reverse_check_eof(cunit* cu){
    if(cu->tknzr.pos == cu->tknzr.str){
        printf("Failed to reverse token: reached end of file\n");
        exit(-1);
    }
}
static inline void reverse_whitespace(cunit* cu){
    char c;
    do{
        reverse_check_eof(cu);
        cu->tknzr.pos--;
    }while(false);
}
static void reverse_string(cunit* cu){
    char c;
    do{
        reverse_check_eof(cu);
        cu->tknzr.pos--;
        c = *cu->tknzr.pos;
    }while ((c >= 'a' && c<= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') ||
            c == '_');
}
static void reverse_char(cunit* cu, char c){
    reverse_check_eof(cu);
    cu->tknzr.pos--;
    if(*cu->tknzr.pos != c){
        printf("Failed to reverse token: file token does not match\n");
        exit(-1);
    }
}
void reverse_token(cunit* cu, token* t){
    const char* s = token_strings[t->type];
    if(s != 0){
        size_t l = strlen(s) - 1;
        while(l != 0){
            l--;
            reverse_char(cu, s[l]);
        }
    }
    else{
        if(t->type == TOKEN_STRING)  reverse_string(cu);
        else if(t->type == TOKEN_BINARY_LITERAL){
            reverse_char(cu, '\'');
            reverse_string(cu);
            reverse_char(cu, '\'');
        }
        else if(t->type == TOKEN_LITERAL){
            reverse_char(cu, '\"');
            reverse_string(cu);
            reverse_char(cu, '\"');
        }
        else if(t->type == TOKEN_NUMBER){
            cu->tknzr.pos--;
            while(*cu->tknzr.pos >= '0' && *cu->tknzr.pos <= '9')cu->tknzr.pos--;
        }
        else{
            printf("Failed to reverse token: unknown token\n");
            exit(-1);
        }
    }
}
void reverse_until_token(cunit* cu, token* t){
    const char* s = token_strings[t->type];
    if(s != 0){
        size_t l = strlen(s) - 1;

    }
}
 */