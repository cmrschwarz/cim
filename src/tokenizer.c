#include "parser.h"
#include "tokenizer.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "error.h"
#include "compiler.h"
#include "tokens.h"
#include <stdarg.h>
#define ASSERT_NEOF(c) do{if((c) == '\0'){CIM_ERROR("Unexpected EOF");}}while(0)
static void populate_file_buffer(cunit* cu, u8* pop_start){
    ureg s = fread(pop_start,
                   1, cu->tknzr.file_buffer.end - pop_start,
                   cu->tknzr.file);
    cu->tknzr.file_buffer.head =  pop_start + s;
}
void tokenizer_start_new_line(cunit *cu){
    cu->tknzr.unloaded_columns_of_line = 0;
    cu->tknzr.line_start = cu->tknzr.curr;
    cu->tknzr.line_number++;
    cu->tknzr.line_comment_offset = 0;
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
    cu->tknzr.filename = filename;
    cu->tknzr.line_number = 0;
    cu->tknzr.unloaded_columns_of_line = 0;
    cu->tknzr.line_start = (char*)cu->tknzr.file_buffer.start;
    cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
    populate_file_buffer(cu, cu->tknzr.file_buffer.start);
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
        cu->tknzr.unloaded_columns_of_line +=
                cu->tknzr.curr - cu->tknzr.line_start;
        cu->tknzr.line_start = (char*)cu->tknzr.file_buffer.start;
        cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
        populate_file_buffer(cu, cu->tknzr.file_buffer.start);
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
            if(*str_start >= cu->tknzr.line_start){
                cu->tknzr.unloaded_columns_of_line+= *str_start - cu->tknzr.line_start;
                cu->tknzr.line_start = (char*)cu->tknzr.file_buffer.start;
            }
            else{
                ureg diff = (u8*)*str_start - cu->tknzr.file_buffer.start;
                cu->tknzr.line_start -= diff;
            }
            cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start + size;
            memmove(cu->tknzr.file_buffer.start, *str_start, size);
            populate_file_buffer(cu, (u8*)cu->tknzr.curr);
            if ((u8 *) cu->tknzr.curr != cu->tknzr.file_buffer.head) {
                *str_start = (char *) cu->tknzr.file_buffer.start;
                return *cu->tknzr.curr;
            }
        }
        //not enough space
        ureg line_offs = (u8*)cu->tknzr.line_start - cu->tknzr.file_buffer.start;
        ureg curr_offs = (u8*)cu->tknzr.curr - cu->tknzr.file_buffer.start;
        dbuffer_grow(&cu->tknzr.file_buffer);
        cu->tknzr.line_start = (char*)cu->tknzr.file_buffer.start + line_offs;
        populate_file_buffer(cu, cu->tknzr.file_buffer.start + size);
        cu->tknzr.curr = (char*)(cu->tknzr.file_buffer.start + curr_offs);
        *str_start = (char*)cu->tknzr.file_buffer.start;
        if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head) {
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
            tokenizer_start_new_line(cu);
            curr = peek_char(cu);
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
        case '\0':tok->type = TOKEN_EOF;return;
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
                    while(curr == '\\'){
                        void_peek(cu);
                        curr = peek_char(cu);
                        ASSERT_NEOF(curr);
                        void_peek(cu);
                        curr = peek_char(cu);
                        ASSERT_NEOF(curr);
                    }
                }while(curr != '\n');
                void_peek(cu);
                tokenizer_start_new_line(cu);
                consume_new_token(cu, tok);
                return;
            }
            if(peek == '*'){
                //PERF: this isn't optimal, we could only inc this va
                //when we actually reached a new line
                bool new_line = false;
                ureg line_leading_comment_columns = 0;
                do{
                    do{
                        void_peek(cu);
                        line_leading_comment_columns++;
                        curr = peek_char(cu);
                        while(curr == '\\'){
                            void_peek(cu);
                            curr = peek_char(cu);
                            ASSERT_NEOF(curr);
                            void_peek(cu);
                            line_leading_comment_columns+=2;
                            curr = peek_char(cu);
                        }
                        while(curr == '\n'){
                            void_peek(cu);
                            tokenizer_start_new_line(cu);
                            curr = peek_char(cu);
                            line_leading_comment_columns =0;
                            new_line = true;
                        }
                        ASSERT_NEOF(curr);
                    }while(curr != '*');
                    void_peek(cu);
                    line_leading_comment_columns++;
                    peek = peek_char(cu);
                }while(peek != '/');
                void_peek(cu);
                line_leading_comment_columns++;
                if(new_line){
                    cu->tknzr.line_comment_offset =
                        line_leading_comment_columns;
                }
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
		default:{
            printf("unknown token: %c", curr);
        }assert(false);
	}
}
ureg get_pos_in_file(cunit* cu){
    long r = ftell(cu->tknzr.file);
    if(r < 0)CIM_ERROR("File IO Error");
    return (ureg)r;
}
ureg get_curr_src_line(cunit* cu){
    return cu->tknzr.line_number;
}
ureg get_curr_src_column(cunit* cu) {
    return cu->tknzr.curr - cu->tknzr.line_start +
           cu->tknzr.unloaded_columns_of_line +
           cu->tknzr.line_comment_offset;
}
char* get_curr_src_file_name(cunit* cu){
    return cu->tknzr.filename;
}
ureg setup_entire_line(cunit* cu){
    char* line_start;
    if(cu->tknzr.unloaded_columns_of_line != 0){
        ureg size_after_curr = cu->tknzr.file_buffer.head - (u8*)cu->tknzr.curr;
        fseek(cu->tknzr.file,
              -(size_after_curr + get_curr_src_column(cu)),
              SEEK_CUR);
        cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
        cu->tknzr.unloaded_columns_of_line = 0;
        //tknzr.line_start gets set by populate_file_buffer
        populate_file_buffer(cu, cu->tknzr.file_buffer.start);
        line_start =  cu->tknzr.curr;
    }
    else{
        line_start = cu->tknzr.line_start;
    }
    char c;
    do{
        c = peek_string_char(cu, &line_start);
        void_peek(cu);
    }while(c!='\n' && c!= '\0');
    ureg size = cu->tknzr.curr - line_start - 1;
    cu->tknzr.curr = line_start;
    return size;
}

void get_token_range(cunit* cu, ureg tbase_column_start, int offs, ureg cnt,
                         ureg* start, ureg* end)
{
    if(offs > 0){
        tbase_column_start+= offs;
        offs = 0;
    }
    char* curr_backup= cu->tknzr.curr;
    char* tgt_column = cu->tknzr.line_start + tbase_column_start;
    char* offs_column = cu->tknzr.line_start + cu->tknzr.line_comment_offset;
    cu->tknzr.curr = offs_column;
    token t;
    while(offs!=0){
        if(*cu->tknzr.curr == '\n'){
            CIM_ERROR("Unexpected end of line");
            return;
        }
        consume_new_token(cu, &t);
        if(t.type == TOKEN_EOF)CIM_ERROR("Unexpected EOF");
        offs++;
    }
    while(cu->tknzr.curr != tgt_column){
        if(*cu->tknzr.curr == '\n'){
            CIM_ERROR("Unexpected end of line");
            return;
        }
        offs_column = cu->tknzr.curr;
        consume_new_token(cu, &t);
        if(t.type == TOKEN_EOF)CIM_ERROR("Unexpected EOF");
    }
    while(*offs_column == ' ')offs_column++;
    *start = offs_column - cu->tknzr.line_start;
    cu->tknzr.curr = offs_column;
    for(ureg i =0;i<cnt;i++){
        if(*cu->tknzr.curr == '\n'){
            CIM_ERROR("Unexpected end of line");
            return;
        }
        consume_new_token(cu, &t);
    }
    *end = cu->tknzr.curr - cu->tknzr.line_start;
    cu->tknzr.curr = curr_backup;
}
const char* get_token_type_str(token_type t){
    static char buff[6] = {'\''};
    if(token_strings[t]!=0){
        ureg len = strlen(token_strings[t]);
        memcpy(buff + 1, token_strings[t], len);
        buff[len+1]='\'';
        return buff;
    }
    else{
        switch  (t){
            case TOKEN_EOF: return "end of file";
            case TOKEN_NUMBER: return "number";
            case TOKEN_STRING: return "string";
            case TOKEN_BINARY_LITERAL: return "binary literal";
            case TOKEN_LITERAL: return "literal";
            default: CIM_ERROR("Failed to print the desired token");
        }
    }
};
const char* make_token_string(cunit* cu, token* t){
    if(token_strings[t->type]!=0)return get_token_type_str(t->type);
    ureg len = strlen(t->str);
    char* buff = sbuffer_append(&cu->data_store, len +3);
    cu->data_store.last->head = (u8*)buff;
    memcpy(buff + 1, t->str, len);
    buff[len+2] = '\0';
    switch (t->type){
        case TOKEN_EOF: return "end of file";
        case TOKEN_LITERAL:
        case TOKEN_NUMBER:
        case TOKEN_STRING: buff[0]='"';buff[len+1]='"';return buff;
        case TOKEN_BINARY_LITERAL: buff[0]='\'';buff[len+1]='\'';return buff;
        default: CIM_ERROR("Failed to print the desired token");
    }
};
void error(cunit* cu, int offs, int cnt, char* str, ...){
    ureg pos = get_curr_src_column(cu);
    ureg line_size = setup_entire_line(cu);
    ureg st, en;
    get_token_range(cu, pos, offs-1-get_lookahead_count(cu), 1, &st, &en);
    printf("%s:%llu:%llu: ",
               get_curr_src_file_name(cu),
               get_curr_src_line(cu) + 1,
               st + 1);
    va_list vl;
    va_start(vl, str);
    vprintf(str, vl);
    va_end(vl);
    putchar('\n');
    for(ureg i = 0; i<line_size;i++){
        putchar(cu->tknzr.curr[i]);
    }
    putchar('\n');
    for(ureg i = 0; i!= st; i++)putchar(' ');
    for(ureg i = st; i!= en; i++)putchar('^');
    fflush(stdout);
    CIM_EXIT;
}