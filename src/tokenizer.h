#pragma once
#include "compiler.h"
#include "token_strings.h"
#include "memory.h"
#include "tokenizer_t.h"

char* store_string(cunit* cu, char* str, char* str_end);
void add_keyword(cunit* cu, const char* str);
const char* get_token_type_str(token_type t);
const char* make_token_string(cunit*cu, token* t);
void tokenizer_open_file(cunit* cu, char* filename);
void tokenizer_close_file(cunit* cu);
void display_string_store(cunit* cu);
void consume_new_token(cunit* cu, token* tok, token* next);
void error(cunit* cu, token* t, ureg incl_prev, ureg underline_prev, char* str, ...);
static inline void inc_token_buff_ptr(cunit* cu, token** p){
    if(*p != &cu->tknzr.token_buffer[TOKEN_BUFFER_SIZE - 1]){
        (*p)++;
    }
    else{
        *p = &cu->tknzr.token_buffer[0];
    }
}
static inline void dec_token_buff_ptr(cunit* cu, token** p){
    if(*p != &cu->tknzr.token_buffer[0]){
        (*p)--;
    }
    else{
        *p = &cu->tknzr.token_buffer[TOKEN_BUFFER_SIZE - 1];
    }
}
static inline token* consume_token(cunit* cu){
    token* s = cu->tknzr.token_start;
    if(cu->tknzr.token_start != cu->tknzr.token_end){
        inc_token_buff_ptr(cu, &cu->tknzr.token_start);
        return s;
    }
    else{
        inc_token_buff_ptr(cu, &cu->tknzr.token_start);
        consume_new_token(cu, s, cu->tknzr.token_start);
        cu->tknzr.token_end = cu->tknzr.token_start;
        return s;
    }
}
static inline token* peek_token(cunit* cu){
     if(cu->tknzr.token_start != cu->tknzr.token_end) {
        return cu->tknzr.token_start;
     }
     else{
         inc_token_buff_ptr(cu, &cu->tknzr.token_end);
         consume_new_token(cu, cu->tknzr.token_start, cu->tknzr.token_end);
         return cu->tknzr.token_start;
     }
}
static inline token* peek_nth_token(cunit* cu, int n){
    n--;
    token* p = cu->tknzr.token_start;
    int i = 0;
    while(i!=n && p != cu->tknzr.token_end){
        inc_token_buff_ptr(cu, &p);
        i++;
    }
    if(i != n){
        do{
            inc_token_buff_ptr(cu, &p);
            consume_new_token(cu, cu->tknzr.token_end, p);
            cu->tknzr.token_end = p;
            i++;
        }while(i != n);
    }
    else{
        if(p == cu->tknzr.token_end){
            inc_token_buff_ptr(cu, &cu->tknzr.token_end);
            consume_new_token(cu, p, cu->tknzr.token_end);
        }
    }
    return p;
}
static inline token* peek_2nd_token(cunit* cu){
    //PERF: maybe optimize this?
    return peek_nth_token(cu, 2);
}
static inline void void_lookahead_token(cunit* cu){
    inc_token_buff_ptr(cu, &cu->tknzr.token_start);
}
static inline void void_lookahead_tokens(cunit *cu, int n){
    for(int i = 0; i< n; i++)inc_token_buff_ptr(cu, &cu->tknzr.token_start);
}
