#include "parser.h"
#include "tokenizer.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "error.h"
#include "compiler.h"
#include "token_strings.h"
#include <stdarg.h>
static inline void comment_assert_neof(cunit* cu, token* t, char c){
    if(c == '\0'){
        token* next = t;
        inc_token_buff_ptr(cu, &next);
        tokenizing_error(cu, t,t->line - next->line, "tokenizing error: unexpected end of file");
    }
}
static inline void assert_neof(cunit* cu, token* t, char c){
    if(c == '\0'){
        tokenizing_error(cu, t, 0,"tokenizing error: unexpected end of file");
    }
}

static void populate_file_buffer(cunit* cu, u8* pop_start){
    ureg s = fread(pop_start,
                   1, cu->tknzr.file_buffer.end - pop_start,
                   cu->tknzr.file);
    cu->tknzr.file_buffer.head =  pop_start + s;
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
    cu->tknzr.token_start = cu->tknzr.token_buffer;
    cu->tknzr.token_end = cu->tknzr.token_start;
    cu->tknzr.token_start->column = 0;
    cu->tknzr.token_start->line = 0;
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
        if(cu->tknzr.file_buffer.start == cu->tknzr.file_buffer.head)return '\0';
        cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start;
        populate_file_buffer(cu, cu->tknzr.file_buffer.start);
        if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head){
            return *cu->tknzr.curr;
        }
        else{
            *cu->tknzr.curr = '\0';
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
        if(cu->tknzr.file_buffer.start == cu->tknzr.file_buffer.head)return '\0';
        ureg size = cu->tknzr.curr - *str_start;
        if((u8*)*str_start != cu->tknzr.file_buffer.start) {
            cu->tknzr.curr = (char*)cu->tknzr.file_buffer.start + size;
            memmove(cu->tknzr.file_buffer.start, *str_start, size);
            populate_file_buffer(cu, (u8*)cu->tknzr.curr);
            if ((u8 *) cu->tknzr.curr != cu->tknzr.file_buffer.head) {
                *str_start = (char *) cu->tknzr.file_buffer.start;
                return *cu->tknzr.curr;
            }
        }
        //not enough space
        ureg curr_offs = (u8*)cu->tknzr.curr - cu->tknzr.file_buffer.start;
        dbuffer_grow(&cu->tknzr.file_buffer);
        populate_file_buffer(cu, cu->tknzr.file_buffer.start + size);
        cu->tknzr.curr = (char*)(cu->tknzr.file_buffer.start + curr_offs);
        *str_start = (char*)cu->tknzr.file_buffer.start;
        if((u8*)cu->tknzr.curr != cu->tknzr.file_buffer.head) {
            return *cu->tknzr.curr;
        }
        else{
            *cu->tknzr.curr = '\0';
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
    ureg siz = str_end - str;
    char* s = sbuffer_append(&cu->data_store, siz + 1);
    s[siz] = '\0';
    memcpy(s, str, siz);
    return s;
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

void consume_new_token(cunit* cu, token* tok, token* next){
    char curr = peek_char(cu);
    next->line = tok->line;
    if(curr == '\0'){
        next->column=tok->column;
        tok->type = TOKEN_EOF;
        return;
    };
    void_peek(cu);

    switch(curr){
        case '$': tok->type = TOKEN_DOLLAR;break;
        case '(': tok->type = TOKEN_PAREN_OPEN;break;
        case ')': tok->type = TOKEN_PAREN_CLOSE;break;
        case '{': tok->type = TOKEN_BRACE_OPEN;break;
        case '}': tok->type = TOKEN_BRACE_CLOSE;break;
        case '[': tok->type = TOKEN_BRACKET_OPEN;break;
        case ']': tok->type = TOKEN_BRACKET_CLOSE;break;
        case ',': tok->type = TOKEN_COMMA;break;
        case ';': tok->type = TOKEN_SEMICOLON; break;
        case '.': tok->type = TOKEN_DOT; break;
        case ':': tok->type = TOKEN_COLON; break;
        case '\t': {
            tok->column++;
            curr = peek_char(cu);
            while(curr == '\t'){
                tok->column++;
                void_peek(cu);
                curr = peek_char(cu);
            }
            return consume_new_token(cu, tok, next);
        }
        case ' ': {
            tok->column++;
            curr = peek_char(cu);
            while(curr == ' '){
                tok->column++;
                void_peek(cu);
                curr = peek_char(cu);
            }
            return consume_new_token(cu, tok, next);
        }
        case '\n':{
            tok->line++;
            tok->column=0;
            return consume_new_token(cu, tok, next);
        }
        case '*': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_STAR_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_STAR;
                break;
            }
        }
        case '+': {
            char peek = peek_char(cu);
            if(peek == '+') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_PLUS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PLUS_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_PLUS;
                break;
            }
        }
        case '-': {
            char peek = peek_char(cu);
            if(peek == '-') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_MINUS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_MINUS_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '>'){
                void_peek(cu);
                tok->type = TOKEN_ARROW;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_MINUS;
                break;
            }
        }
        case '!': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_EXCLAMATION_MARK_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_EXCLAMATION_MARK;
                break;
            }
        }
        case '|': {
            char peek = peek_char(cu);
            if(peek == '|') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_PIPE;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PIPE_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_PIPE;
                break;
            }
        }
        case '&': {
            char peek = peek_char(cu);
            if(peek == '&') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_AND;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_AND_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_AND;
                break;
            }
        }
        case '^': {
            char peek = peek_char(cu);
            if(peek == '^') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_CARET;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_CARET_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_CARET;
                break;
            }
        } return;
        case '~': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_TILDE_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_TILDE;
                break;
            }
        }
       case '=': {
            char peek = peek_char(cu);
            if(peek == '=') {
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                 tok->type = TOKEN_EQUALS;
                break;
            }
        }
        case '/': {
            char peek = peek_char(cu);
            if(peek == '/'){
                do{
                    void_peek(cu);
                    curr = peek_char(cu);
                    while(curr == '\\'){
                        void_peek(cu);
                        curr = peek_char(cu);
                        comment_assert_neof(cu, tok, curr);
                        void_peek(cu);
                        curr = peek_char(cu);
                        comment_assert_neof(cu, tok, curr);
                    }
                }while(curr != '\n' && curr != '\0');
                void_peek(cu);
                if(curr == '\n'){
                    tok->line++;
                    tok->column = 0;
                    return consume_new_token(cu, tok, next);
                }
                tok->type = TOKEN_EOF;
                return;
            }
            if(peek == '*'){
                tok->column+=2;
                void_peek(cu);
                curr = peek_char(cu);
                do{
                    do{
                        tok->column++;
                        if(curr == '\\'){
                            void_peek(cu);
                            curr = peek_char(cu);
                            comment_assert_neof(cu, tok, curr);
                            tok->column++;
                        }
                        if(curr == '\n'){
                            tok->line++;
                            tok->column = 0;
                        }
                        comment_assert_neof(cu, tok, curr);
                        void_peek(cu);
                        curr = peek_char(cu);
                    }while(curr != '*');
                    tok->column++;
                    void_peek(cu);
                    peek = peek_char(cu);
                }while(peek != '/');
                void_peek(cu);
                tok->column++;
                return consume_new_token(cu, tok, next);
            }
            if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_SLASH_EQUALS;
                next->column= tok->column + 2;
                return;
            }
            else{
                tok->type = TOKEN_SLASH;
                break;
            }
        }
        case '%': {
            char peek = peek_char(cu);
            if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_PERCENT_EQUALS;
                next->column = tok->column + 2;
                return;
            }
            else{
                tok->type = TOKEN_PERCENT;
                break;
            }
        }
        case '#': {
            char peek = peek_char(cu);
            if(peek == '#'){
                void_peek(cu);
                tok->type = TOKEN_DOUBLE_HASH;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_HASH;
                break;
            }
        }
        case '<': {
            char peek = peek_char(cu);
            if(peek == '<'){
                void_peek(cu);
                peek = peek_char(cu);
                if(peek == '='){
                    void_peek(cu);
                    tok->type = TOKEN_DOUBLE_LESS_THAN_EQUALS;
                    next->column = tok->column+3;
                    return;
                }
                else{
                    tok->type = TOKEN_DOUBLE_LESS_THAN;
                    next->column = tok->column+2;
                    return;
                }
            }
            else if(peek == '='){
                void_peek(cu);
                tok->type = TOKEN_LESS_THAN_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_LESS_THAN;
                break;
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
                    next->column = tok->column+3;
                    return;
                }
                else{
                    tok->type = TOKEN_DOUBLE_GREATER_THAN;
                    next->column = tok->column+2;
                    return;
                }
            }
            else if(peek == '='){
                tok->type = TOKEN_GREATER_THAN_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_GREATER_THAN;
                break;
            }
        } return;
        case '\'':{
            char* str_start = cu->tknzr.curr;
             curr = peek_string_char(cu, &str_start);
            do{
                next->column++;
                assert_neof(cu, tok, curr);
                if(curr == '\\'){
                    //TODO: think about handling escaped chars
                    void_peek(cu);
                    curr = peek_string_char(cu, &str_start);
                    assert_neof(cu, tok, curr);
                    next->column++;
                }
                if(curr == '\n'){
                    curr = peek_string_char(cu, &str_start);
                    assert_neof(cu, tok, curr);
                }
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
            }while(curr != '\'');
            tok->str = store_string(cu, str_start, cu->tknzr.curr);
            void_peek(cu);
            tok->type = TOKEN_BINARY_LITERAL;
        } return;
        case '\"':{
            char* str_start = cu->tknzr.curr;
            do{
                next->column++;
                curr = peek_string_char(cu, &str_start);
                assert_neof(cu, tok, curr);
                if(curr == '\\'){
                    //TODO: think about handling escaped chars
                    void_peek(cu);
                    curr = peek_string_char(cu, &str_start);
                    assert_neof(cu, tok, curr);
                    void_peek(cu);
                    curr = peek_string_char(cu, &str_start);
                    assert_neof(cu, tok, curr);
                    next->column+=2;
                }
                if(curr == '\n'){
                    curr = peek_string_char(cu, &str_start);
                    assert_neof(cu, tok, curr);
                }
                void_peek(cu);
            }while(curr != '\"');
            tok->str = store_string(cu, str_start, cu->tknzr.curr);
            tok->type = TOKEN_LITERAL;
        } return;
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case '_':
        {
            char* str_start = cu->tknzr.curr-1;
            curr = peek_string_char(cu, &str_start);
            while((curr >= 'a' && curr <= 'z') ||
                   (curr >= 'A' && curr <= 'Z') ||
                   (curr >= '0' && curr <= '9') ||
                   (curr == '_'))
            {
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
            }
            tok->str = store_string(cu, str_start, cu->tknzr.curr);
            next->column = tok->column + cu->tknzr.curr - str_start;
            tok->type = TOKEN_STRING;
            return;
        }
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        {
            char* str_start = cu->tknzr.curr-1;
            curr = peek_string_char(cu, &str_start);
            while(curr >= '0' && curr <= '9'){
                void_peek(cu);
                curr = peek_string_char(cu, &str_start);
               assert_neof(cu, tok, curr);
            }
            tok->str = store_string(cu, str_start, cu->tknzr.curr);
            next->column = tok->column + cu->tknzr.curr - str_start;
            tok->type = TOKEN_NUMBER;
        } return;
        default:{
            tokenizing_error(cu, tok,0, "tokenizing error: unknown token");
        }
    }
    next->column = tok->column+1;
}
const char* get_token_type_str(cunit* cu, token_type t){
    if(token_strings[t]!=0){
        ureg len = strlen(token_strings[t]);
        char* buff = sbuffer_append(&cu->data_store, len + 3);
        buff[0]='\'';
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
    if(token_strings[t->type]!=0)return get_token_type_str(cu, t->type);
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
static void revert_n_lines(cunit* cu, ureg subtract_lines){
    subtract_lines++;
    long fpos = ftell(cu->tknzr.file);
    if(fpos < 0)CIM_ERROR("file IO error");
    char* strp = cu->tknzr.curr;
    if(*strp == '\n')subtract_lines++;
    while(subtract_lines != 0){
        bool newline_found = false;
        while(newline_found == false){
            if(*strp == '\n'){
                if(subtract_lines==1)break;
                newline_found = true;
            }
            if (strp == (char *) cu->tknzr.file_buffer.start) {
                if (fpos == 0)break;
                long off = cu->tknzr.file_buffer.head - cu->tknzr.file_buffer.start;
                long loff = off +
                            (cu->tknzr.file_buffer.end - cu->tknzr.file_buffer.start);
                if ((fpos - loff)< 0) {
                    if(off == fpos){
                        if(subtract_lines==1)break;
                        CIM_ERROR("Unexpected begin of file");
                    }
                    fseek(cu->tknzr.file, 0, SEEK_SET);
                    populate_file_buffer(cu, cu->tknzr.file_buffer.start);
                    strp = (char*)(cu->tknzr.file_buffer.start + fpos - 1);
                    fpos = 0;
                } else {
                    fpos -= loff;
                    fseek(cu->tknzr.file, -loff, SEEK_CUR);
                    populate_file_buffer(cu, cu->tknzr.file_buffer.start);
                    strp = (char*)(cu->tknzr.file_buffer.head - 1);
                }

            }
            else {
                strp--;
            }
        }
        subtract_lines--;
    }
    cu->tknzr.curr = strp;
    if(*strp == '\n' && cu->tknzr.file_buffer.head != (u8*)cu->tknzr.curr){
        cu->tknzr.curr++;
    }
    return;
}
void tokenizing_error(cunit* cu, token* t,ureg lines_to_incl, char* str, ...){
    printf("%s:%llu:%llu: ",
               cu->tknzr.filename,
               t->line + 1,
               t->column);
    va_list vl;
    va_start(vl, str);
    vprintf(str, vl);
    va_end(vl);
    putchar('\n');
    revert_n_lines(cu, lines_to_incl);
    ureg lc = 0;
    char c = peek_char(cu);
    for(ureg i = 0; i!= lines_to_incl;i++){
        while(c != '\n'){
            putchar(c);
            void_peek(cu);
            c = peek_char(cu);
        }
        void_peek(cu);
        c = peek_char(cu);
        putchar('\n');
    }
    while(c != '\n' && c!= '\0'){
        putchar(c);
        void_peek(cu);
        c = peek_char(cu);
        lc++;
    }
    putchar('\n');
    for(ureg i = 0; i!= t->column; i++)putchar(' ');
    for(ureg i = t->column; i!= t->column + 1; i++)putchar('^');
    putchar('\n');
    void_peek(cu);
    fflush(stdout);
    CIM_EXIT;
}
void syntax_error(cunit* cu, token* t, ureg incl_prev, ureg underline_prev, char* str, ...){
    clear_lookahead(cu);
    token* prev_incl= t;
    token* prev_underl= t;
    for(ureg i = 0;i!=incl_prev;i++) dec_token_buff_ptr(cu, &prev_incl);
    for(ureg i = 0;i!=underline_prev;i++) dec_token_buff_ptr(cu, &prev_underl);
    token* next = t;
    inc_token_buff_ptr(cu, &next);
    ureg line_size;
    ureg entire_size;
    printf("%s:%llu:%llu: ",
               cu->tknzr.filename,
               t->line + 1,
               t->column);
    va_list vl;
    va_start(vl, str);
    vprintf(str, vl);
    va_end(vl);
    putchar('\n');
    char* curr_backup = cu->tknzr.curr;
    ureg line_sub = next->line - prev_incl->line;
    revert_n_lines(cu, line_sub);
    ureg lines_pre_underline = prev_underl->line - prev_incl->line;
    char c;
    for(ureg i = 0;i!=lines_pre_underline;i++){
        c = peek_char(cu);
        while(c != '\n'){
            putchar(c);
            void_peek(cu);
            c = peek_char(cu);
        }
        putchar('\n');
        void_peek(cu);
    }
    if(prev_underl->line == t->line){
        for(ureg i = 0; i!= t->column; i++) {
            c = peek_char(cu);
            putchar(c);
            void_peek(cu);
        }
        char* t_start = cu->tknzr.curr;
        //TODO: maybe reverse again instead
        c = peek_string_char(cu, &t_start);
        while(c != '\n' && c !='\0'){
            putchar(c);
            void_peek(cu);
            c = peek_string_char(cu, &t_start);
        }
        putchar('\n');
        cu->tknzr.curr = t_start;
        token* tok = consume_token(cu);
        ureg ts = cu->tknzr.curr - t_start;
        if(tok->type == TOKEN_EOF)ts++;
        for(ureg i = 0; i< prev_underl->column; i++)putchar(' ');
        for(ureg i = prev_underl->column; i != t->column + ts; i++)putchar('^');
    }
    else{
        ureg lc = 0;
        do {
            c = peek_char(cu);
            putchar(c);
            void_peek(cu);
            lc++;
        }while(c != '\n');
        for(ureg i = 0; i != prev_underl->column; i++)putchar(' ');
        for(ureg i = prev_underl->column; i< lc-1; i++)putchar('^');
        putchar('\n');
        ureg ld = t->line - prev_underl->line - 1;
        for(ureg l = 0; l!=ld;l++){
            lc = 0;
            c = peek_char(cu);
            while(c != '\n' && c !='\0'){
                putchar(c);
                void_peek(cu);
                c = peek_char(cu);
                lc++;
            }
            if(c!='\0') void_peek(cu);
            putchar('\n');
            for(ureg i = 0; i!= lc; i++)putchar('^');
            putchar('\n');
        }
        lc = 0;
        while(lc!= t->column) {
            c = peek_char(cu);
            putchar(c);
            void_peek(cu);
            lc++;
        }
        char* t_start = cu->tknzr.curr;
        //TODO: maybe reverse again instead
        c = peek_string_char(cu, &t_start);
        while(c != '\n' && c !='\0'){
            putchar(c);
            void_peek(cu);
            c = peek_string_char(cu, &t_start);
        }
        putchar('\n');
        void_peek(cu);
        cu->tknzr.curr = t_start;
        token* tok = consume_token(cu);
        ureg ts = cu->tknzr.curr - t_start;
        if(tok->type == TOKEN_EOF)ts++;
        for(ureg i = 0; i!= t->column + ts; i++)putchar('^');
        for(ureg i = t->column + ts; i!= t->column + ts + lc; i++)putchar(' ');
    }
    putchar('\n');
    fflush(stdout);
    tokenizer_close_file(cu);
    CIM_EXIT;
}
