#include "tokenizer.h"
#include "error.h"
#include <stdarg.h>
static inline void comment_assert_neof(tokenizer* tk, token* t, char c){
    if(c == '\0'){
        token* next = t;
        inc_token_buff_ptr(tk, &next);
        tokenizing_error(tk, t,t->line - next->line, "tokenizing error: unexpected end of file");
    }
}
static inline void assert_neof(tokenizer* tk, token* t, char c){
    if(c == '\0'){
        tokenizing_error(tk, t, 0,"tokenizing error: unexpected end of file");
    }
}

void tokenizer_init(tokenizer* t){
    sbuffer_init(&t->string_store, 4);
    dbuffer_init_with_capacity(&t->file_buffer, 8192);
    t->file = NULL;
}
void tokenizer_fin(tokenizer* t){
    sbuffer_fin(&t->string_store);
    dbuffer_fin(&t->file_buffer);
    if(t->file != NULL)tokenizer_close_file(t);
}

static void populate_file_buffer(tokenizer* tk, u8* pop_start){
    ureg s = fread(pop_start,
                   1, tk->file_buffer.end - pop_start,
                   tk->file);
    tk->file_buffer.head =  pop_start + s;
}
void tokenizer_open_file(tokenizer* tk, char* filename) {
    if(tk->file != NULL){
        fclose(tk->file);
    }
    tk->file = fopen(filename, "r");
    if(tk->file == NULL){
        printf("Failed to open file %s", filename);
        exit(-1);
    }
    tk->filename = filename;
    tk->token_start = tk->token_buffer;
    tk->token_start->line = 0;
    tk->token_start->column = 0;
    inc_token_buff_ptr(tk, &tk->token_start);
    tk->token_end = tk->token_start;
    tk->token_start->column = 0;
    tk->token_start->line = 0;
    tk->curr = (char*)tk->file_buffer.start;
    populate_file_buffer(tk, tk->file_buffer.start);
}
void tokenizer_close_file(tokenizer* tk){
    fclose(tk->file);
    tk->filename = NULL;
    tk->file = NULL;
}
static inline void unread_char(tokenizer* tk){
     tk->curr--;
}
static inline char peek_char(tokenizer* tk){
    if((u8*)tk->curr != tk->file_buffer.head){
        return *tk->curr;
    }
    else{
        if(tk->file_buffer.start == tk->file_buffer.head)return '\0';
        tk->curr = (char*)tk->file_buffer.start;
        populate_file_buffer(tk, tk->file_buffer.start);
        if((u8*)tk->curr != tk->file_buffer.head){
            return *tk->curr;
        }
        else{
            *tk->curr = '\0';
            return '\0';
        }
    }
}
static inline void void_peek(tokenizer* tk){
    tk->curr+=1;
}
static inline char peek_string_char(tokenizer* tk, char** str_start){
    if((u8*)tk->curr != tk->file_buffer.head) {
        return *tk->curr;
    }
    else{
        if(tk->file_buffer.start == tk->file_buffer.head)return '\0';
        ureg size = tk->curr - *str_start;
        if((u8*)*str_start != tk->file_buffer.start) {
            tk->curr = (char*)tk->file_buffer.start + size;
            memmove(tk->file_buffer.start, *str_start, size);
            populate_file_buffer(tk, (u8*)tk->curr);
            if ((u8 *) tk->curr != tk->file_buffer.head) {
                *str_start = (char *) tk->file_buffer.start;
                return *tk->curr;
            }
        }
        //not enough space
        ureg curr_offs = (u8*)tk->curr - tk->file_buffer.start;
        dbuffer_grow(&tk->file_buffer);
        populate_file_buffer(tk, tk->file_buffer.start + size);
        tk->curr = (char*)(tk->file_buffer.start + curr_offs);
        *str_start = (char*)tk->file_buffer.start;
        if((u8*)tk->curr != tk->file_buffer.head) {
            return *tk->curr;
        }
        else{
            *tk->curr = '\0';
            return '\0';
        }
    };
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
char* store_string(tokenizer* tk, char* str, char* str_end){
    ureg siz = str_end - str;
    char* s = sbuffer_append(&tk->string_store, siz + 1);
    s[siz] = '\0';
    memcpy(s, str, siz);
    return s;
}

void consume_new_token(tokenizer* tk, token* tok, token* next){
    char curr = peek_char(tk);
    next->line = tok->line;
    if(curr == '\0'){
        next->column=tok->column;
        tok->type = TOKEN_EOF;
        return;
    };
    void_peek(tk);

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
        case '\t': {
            tok->column++;
            curr = peek_char(tk);
            while(curr == '\t'){
                tok->column++;
                void_peek(tk);
                curr = peek_char(tk);
            }
            return consume_new_token(tk, tok, next);
        }
        case ' ': {
            tok->column++;
            curr = peek_char(tk);
            while(curr == ' '){
                tok->column++;
                void_peek(tk);
                curr = peek_char(tk);
            }
            return consume_new_token(tk, tok, next);
        }
        case '\n':{
            tok->line++;
            tok->column=0;
            return consume_new_token(tk, tok, next);
        }
        case ':':{
          char peek = peek_char(tk);
            if(peek == ':'){
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_COLON;
                next->column = tok->column+2;
                return;
            }
            else{
                tok->type = TOKEN_COLON;
                break;
            }
        }
        case '*': {
            char peek = peek_char(tk);
            if(peek == '=') {
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '+') {
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_PLUS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '-') {
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_MINUS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(tk);
                tok->type = TOKEN_MINUS_EQUALS;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '>'){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '=') {
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '|') {
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_PIPE;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '&') {
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_AND;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '^') {
                void_peek(tk);
                tok->type = TOKEN_DOUBLE_CARET;
                next->column = tok->column+2;
                return;
            }
            else if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '=') {
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '=') {
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '/'){
                do{
                    void_peek(tk);
                    curr = peek_char(tk);
                    while(curr == '\\'){
                        void_peek(tk);
                        curr = peek_char(tk);
                        comment_assert_neof(tk, tok, curr);
                        void_peek(tk);
                        curr = peek_char(tk);
                        comment_assert_neof(tk, tok, curr);
                    }
                }while(curr != '\n' && curr != '\0');
                void_peek(tk);
                if(curr == '\n'){
                    tok->line++;
                    tok->column = 0;
                    return consume_new_token(tk, tok, next);
                }
                tok->type = TOKEN_EOF;
                return;
            }
            if(peek == '*'){
                tok->column+=2;
                void_peek(tk);
                curr = peek_char(tk);
                do{
                    do{
                        tok->column++;
                        if(curr == '\\'){
                            void_peek(tk);
                            curr = peek_char(tk);
                            comment_assert_neof(tk, tok, curr);
                            tok->column++;
                        }
                        if(curr == '\n'){
                            tok->line++;
                            tok->column = 0;
                        }
                        comment_assert_neof(tk, tok, curr);
                        void_peek(tk);
                        curr = peek_char(tk);
                    }while(curr != '*');
                    tok->column++;
                    void_peek(tk);
                    peek = peek_char(tk);
                }while(peek != '/');
                void_peek(tk);
                tok->column++;
                return consume_new_token(tk, tok, next);
            }
            if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '='){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '#'){
                void_peek(tk);
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
            char peek = peek_char(tk);
            if(peek == '<'){
                void_peek(tk);
                peek = peek_char(tk);
                if(peek == '='){
                    void_peek(tk);
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
                void_peek(tk);
                peek = peek_char(tk);
                if(peek == '='){
                    void_peek(tk);
                    tok->type = TOKEN_LEFT_ARROW;
                    next->column = tok->column+3;
                    return;
                }
                else{
                    tok->type = TOKEN_LESS_THAN_EQUALS;
                    next->column = tok->column+2;
                    return;
                }

            }
            else{
                tok->type = TOKEN_LESS_THAN;
                break;
            }
        } return;
        case '>': {
            char peek = peek_char(tk);
            if(peek == '>'){
                void_peek(tk);
                peek = peek_char(tk);
                if(peek == '='){
                    void_peek(tk);
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
            char* str_start = tk->curr;
             curr = peek_string_char(tk, &str_start);
            do{
                next->column++;
                assert_neof(tk, tok, curr);
                if(curr == '\\'){
                    //TODO: think about handling escaped chars
                    void_peek(tk);
                    curr = peek_string_char(tk, &str_start);
                    assert_neof(tk, tok, curr);
                    next->column++;
                }
                if(curr == '\n'){
                    curr = peek_string_char(tk, &str_start);
                    assert_neof(tk, tok, curr);
                }
                void_peek(tk);
                curr = peek_string_char(tk, &str_start);
            }while(curr != '\'');
            tok->str = store_string(tk, str_start, tk->curr);
            void_peek(tk);
            tok->type = TOKEN_BINARY_LITERAL;
        } return;
        case '\"':{
            char* str_start = tk->curr;
            do{
                next->column++;
                curr = peek_string_char(tk, &str_start);
                assert_neof(tk, tok, curr);
                if(curr == '\\'){
                    //TODO: think about handling escaped chars
                    void_peek(tk);
                    curr = peek_string_char(tk, &str_start);
                    assert_neof(tk, tok, curr);
                    void_peek(tk);
                    curr = peek_string_char(tk, &str_start);
                    assert_neof(tk, tok, curr);
                    next->column+=2;
                }
                if(curr == '\n'){
                    curr = peek_string_char(tk, &str_start);
                    assert_neof(tk, tok, curr);
                }
                void_peek(tk);
            }while(curr != '\"');
            tok->str = store_string(tk, str_start, tk->curr - 1);
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
            char* str_start = tk->curr-1;
            curr = peek_string_char(tk, &str_start);
            while((curr >= 'a' && curr <= 'z') ||
                   (curr >= 'A' && curr <= 'Z') ||
                   (curr >= '0' && curr <= '9') ||
                   (curr == '_'))
            {
                void_peek(tk);
                curr = peek_string_char(tk, &str_start);
            }
            tok->str = store_string(tk, str_start, tk->curr);
            next->column = tok->column + tk->curr - str_start;
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
            char* str_start = tk->curr-1;
            curr = peek_string_char(tk, &str_start);
            while(curr >= '0' && curr <= '9'){
                void_peek(tk);
                curr = peek_string_char(tk, &str_start);
               assert_neof(tk, tok, curr);
            }
            tok->str = store_string(tk, str_start, tk->curr);
            next->column = tok->column + tk->curr - str_start;
            tok->type = TOKEN_NUMBER;
        } return;
        default:{
            tokenizing_error(tk, tok,0, "tokenizing error: unknown token");
        }
    }
    next->column = tok->column+1;
}
const char* get_token_type_str(tokenizer* tk, token_type t){
    if(token_strings[t]!=0){
        ureg len = strlen(token_strings[t]);
        char* buff = sbuffer_append(&tk->string_store, len + 3);
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
const char* get_token_str(tokenizer* tk, token* t){
    if(token_strings[t->type]!=0)return get_token_type_str(tk, t->type);
    ureg len = strlen(t->str);
    char* buff = sbuffer_append(&tk->string_store, len +3);
    tk->string_store.last->head = (u8*)buff;
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
static void revert_n_lines(tokenizer* tk, ureg subtract_lines){
    subtract_lines++;
    long fpos = ftell(tk->file);
    if(fpos < 0)CIM_ERROR("file IO error");
    char* strp = tk->curr;
    if(*strp == '\n')subtract_lines++;
    while(subtract_lines != 0){
        bool newline_found = false;
        while(newline_found == false){
            if(*strp == '\n'){
                if(subtract_lines==1)break;
                newline_found = true;
            }
            if (strp == (char *) tk->file_buffer.start) {
                if (fpos == 0)break;
                long off = tk->file_buffer.head - tk->file_buffer.start;
                long loff = off +
                            (tk->file_buffer.end - tk->file_buffer.start);
                if ((fpos - loff)< 0) {
                    if(off == fpos){
                        if(subtract_lines==1)break;
                        CIM_ERROR("Unexpected begin of file");
                    }
                    fseek(tk->file, 0, SEEK_SET);
                    populate_file_buffer(tk, tk->file_buffer.start);
                    strp = (char*)(tk->file_buffer.start + fpos - 1);
                    fpos = 0;
                } else {
                    fpos -= loff;
                    fseek(tk->file, -loff, SEEK_CUR);
                    populate_file_buffer(tk, tk->file_buffer.start);
                    strp = (char*)(tk->file_buffer.head - 1);
                }

            }
            else {
                strp--;
            }
        }
        subtract_lines--;
    }
    tk->curr = strp;
    if(*strp == '\n' && tk->file_buffer.head != (u8*)tk->curr){
        tk->curr++;
    }
    return;
}
void tokenizing_error(tokenizer* tk, token* t,ureg lines_to_incl, char* str, ...){
    printf("%s:%llu:%llu: ",
               tk->filename,
               t->line + 1,
               t->column);
    va_list vl;
    va_start(vl, str);
    vprintf(str, vl);
    va_end(vl);
    putchar('\n');
    revert_n_lines(tk, lines_to_incl);
    ureg lc = 0;
    char c = peek_char(tk);
    for(ureg i = 0; i!= lines_to_incl;i++){
        while(c != '\n'){
            putchar(c);
            void_peek(tk);
            c = peek_char(tk);
        }
        void_peek(tk);
        c = peek_char(tk);
        putchar('\n');
    }
    while(c != '\n' && c!= '\0'){
        putchar(c);
        void_peek(tk);
        c = peek_char(tk);
        lc++;
    }
    putchar('\n');
    for(ureg i = 0; i!= t->column; i++)putchar(' ');
    for(ureg i = t->column; i!= t->column + 1; i++)putchar('^');
    putchar('\n');
    void_peek(tk);
    fflush(stdout);
    CIM_EXIT;
}
void syntax_error(tokenizer* tk, token* t, ureg incl_prev, ureg underline_prev, char* str, ...){
    clear_lookahead(tk);
    token* prev_incl= t;
    token* prev_underl= t;
    for(ureg i = 0;i!=incl_prev;i++) dec_token_buff_ptr(tk, &prev_incl);
    for(ureg i = 0;i!=underline_prev;i++) dec_token_buff_ptr(tk, &prev_underl);
    token* next = t;
    inc_token_buff_ptr(tk, &next);
    ureg line_size;
    ureg entire_size;
    printf("%s:%llu:%llu: syntax error: ",
               tk->filename,
               t->line + 1,
               t->column);
    va_list vl;
    va_start(vl, str);
    vprintf(str, vl);
    va_end(vl);
    putchar('\n');
    char* curr_backup = tk->curr;
    ureg line_sub = next->line - prev_incl->line;
    revert_n_lines(tk, line_sub);
    ureg lines_pre_underline = prev_underl->line - prev_incl->line;
    char c;
    for(ureg i = 0;i!=lines_pre_underline;i++){
        c = peek_char(tk);
        while(c != '\n'){
            putchar(c);
            void_peek(tk);
            c = peek_char(tk);
        }
        putchar('\n');
        void_peek(tk);
    }
    if(prev_underl->line == t->line){
        for(ureg i = 0; i!= t->column; i++) {
            c = peek_char(tk);
            putchar(c);
            void_peek(tk);
        }
        char* t_start = tk->curr;
        //TODO: maybe reverse again instead
        c = peek_string_char(tk, &t_start);
        while(c != '\n' && c !='\0'){
            putchar(c);
            void_peek(tk);
            c = peek_string_char(tk, &t_start);
        }
        putchar('\n');
        tk->curr = t_start;
        token* tok = consume_token(tk);
        ureg ts = tk->curr - t_start;
        if(tok->type == TOKEN_EOF)ts++;
        for(ureg i = 0; i< prev_underl->column; i++)putchar(' ');
        for(ureg i = prev_underl->column; i != t->column + ts; i++)putchar('^');
    }
    else{
        ureg lc = 0;
        do {
            c = peek_char(tk);
            putchar(c);
            void_peek(tk);
            lc++;
        }while(c != '\n');
        for(ureg i = 0; i != prev_underl->column; i++)putchar(' ');
        for(ureg i = prev_underl->column; i< lc-1; i++)putchar('^');
        putchar('\n');
        ureg ld = t->line - prev_underl->line - 1;
        for(ureg l = 0; l!=ld;l++){
            lc = 0;
            c = peek_char(tk);
            while(c != '\n' && c !='\0'){
                putchar(c);
                void_peek(tk);
                c = peek_char(tk);
                lc++;
            }
            if(c!='\0') void_peek(tk);
            putchar('\n');
            for(ureg i = 0; i!= lc; i++)putchar('^');
            putchar('\n');
        }
        lc = 0;
        while(lc!= t->column) {
            c = peek_char(tk);
            putchar(c);
            void_peek(tk);
            lc++;
        }
        char* t_start = tk->curr;
        //TODO: maybe reverse again instead
        c = peek_string_char(tk, &t_start);
        while(c != '\n' && c !='\0'){
            putchar(c);
            void_peek(tk);
            c = peek_string_char(tk, &t_start);
        }
        putchar('\n');
        void_peek(tk);
        tk->curr = t_start;
        token* tok = consume_token(tk);
        ureg ts = tk->curr - t_start;
        if(tok->type == TOKEN_EOF)ts++;
        for(ureg i = 0; i!= t->column + ts; i++)putchar('^');
        for(ureg i = t->column + ts; i!= t->column + ts + lc; i++)putchar(' ');
    }
    putchar('\n');
    fflush(stdout);
    tokenizer_close_file(tk);
    CIM_EXIT;
}
