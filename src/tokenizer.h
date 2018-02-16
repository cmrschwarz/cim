#pragma once
#include "tokens.h"
#include "memory.h"
#include "dbuffer.h"
#include "sbuffer.h"
#include <stdio.h>

typedef struct{
    token_type type;
    char* str;
    ureg column;
    ureg line;
}token;
#define TOKEN_BUFFER_SIZE 32
typedef struct {
    char* filename;
    FILE* file;
    char* curr;
    token token_buffer[TOKEN_BUFFER_SIZE];
    token* token_start;
    token* token_end;
    dbuffer file_buffer;
    sbuffer string_store;
}tokenizer;

void tokenizer_init(tokenizer* tk);
void tokenizer_fin(tokenizer* tk);

char* store_string(tokenizer* tk, char* str, char* str_end);
const char* get_token_type_str(tokenizer* tk, token_type t);
const char* get_token_str(tokenizer* tk, token* t);
void tokenizer_open_file(tokenizer* tk, char* filename);
void tokenizer_close_file(tokenizer* tk);
void consume_new_token(tokenizer* tk, token* tok, token* next);
void syntax_error(tokenizer *tk, token *t, ureg incl_prev, ureg underline_prev, char *str, ...);
void tokenizing_error(tokenizer* tk, token* t, ureg lines_to_include, char* str, ...);
static inline bool str_eq_keyword(const char* str,const  char* keyword){
    return strcmp(str, keyword) == 0;
}
static inline void inc_token_buff_ptr(tokenizer* tk, token** t){
    if(*t != &tk->token_buffer[TOKEN_BUFFER_SIZE - 1]){
        (*t)++;
    }
    else{
        *t = &tk->token_buffer[0];
    }
}
static inline void dec_token_buff_ptr(tokenizer* tk, token** t){
    if(*t != &tk->token_buffer[0]){
        (*t)--;
    }
    else{
        *t = &tk->token_buffer[TOKEN_BUFFER_SIZE - 1];
    }
}
static inline token* consume_token(tokenizer* tk){
    token* s = tk->token_start;
    if(tk->token_start != tk->token_end){
        inc_token_buff_ptr(tk, &tk->token_start);
        return s;
    }
    else{
        inc_token_buff_ptr(tk, &tk->token_start);
        consume_new_token(tk, s, tk->token_start);
        tk->token_end = tk->token_start;
        return s;
    }
}
static inline token* peek_token(tokenizer* tk){
     if(tk->token_start != tk->token_end) {
        return tk->token_start;
     }
     else{
         inc_token_buff_ptr(tk, &tk->token_end);
         consume_new_token(tk, tk->token_start, tk->token_end);
         return tk->token_start;
     }
}
static inline token* peek_nth_token(tokenizer* tk, int n){
    n--;
    token* t = tk->token_start;
    int i = 0;
    while(i!=n && t != tk->token_end){
        inc_token_buff_ptr(tk, &t);
        i++;
    }
    if(i != n){
        do{
            inc_token_buff_ptr(tk, &t);
            consume_new_token(tk, tk->token_end, t);
            tk->token_end = t;
            i++;
        }while(i != n);
    }
    else{
        if(t == tk->token_end){
            inc_token_buff_ptr(tk, &tk->token_end);
            consume_new_token(tk, t, tk->token_end);
        }
    }
    return t;
}
static inline void clear_lookahead(tokenizer* tk){
    tk->token_start = tk->token_end;
}
static inline token* peek_2nd_token(tokenizer* p){
    //PERF: maybe optimize this?
    return peek_nth_token(p, 2);
}
static inline void void_lookahead_token(tokenizer* tk){
    inc_token_buff_ptr(tk, &tk->token_start);
}
static inline void void_lookahead_tokens(tokenizer *tk, int n){
    for(int i = 0; i< n; i++)inc_token_buff_ptr(tk, &tk->token_start);
}
