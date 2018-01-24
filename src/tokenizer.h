#pragma once
#include "compiler.h"
#include "tokens.h"
#include "memory.h"
//TODO: evaluate using token pointers instead

void tokenizer_open_file(cunit* cu, char* filename);
void tokenizer_close_file(cunit* cu);
void display_string_store(cunit* cu);
void consume_new_token(cunit* cu, token* t);
static inline void void_lookahead_token(cunit* cu){
    cu->tknzr.lookahead_head--;
    if(cu->tknzr.lookahead_head != cu->tknzr.lookahead_store){
        token* s = cu->tknzr.lookahead_store;
        do{*s = *(s+1);s++;}while(s != cu->tknzr.lookahead_head);
    }
}
static inline void void_2_lookahead_tokens(cunit* cu){
    cu->tknzr.lookahead_head-=2;
    if(cu->tknzr.lookahead_head != cu->tknzr.lookahead_store){
        token* s = cu->tknzr.lookahead_store;
        do{*s = *(s+1);s++;}while(s != cu->tknzr.lookahead_head);
    }
}
static inline void consume_lookahead_token(cunit* cu, token* t){
    *t =  cu->tknzr.lookahead_store[0];
    void_lookahead_token(cu);
}
static inline void peek_token(cunit *cu, token *t){
    if(cu->tknzr.lookahead_head == cu->tknzr.lookahead_store){
        consume_new_token(cu, cu->tknzr.lookahead_head);
        cu->tknzr.lookahead_head++;
    }
    *t = *cu->tknzr.lookahead_store;
}
static inline void peek_2nd_token(cunit *cu, token *t){
    while(cu->tknzr.lookahead_head < cu->tknzr.lookahead_store + 2){
        consume_new_token(cu, cu->tknzr.lookahead_head);
        cu->tknzr.lookahead_head++;
    }
    *t = cu->tknzr.lookahead_store[1];
}
static inline void peek_3rd_token(cunit *cu, token *t){
    while(cu->tknzr.lookahead_head < cu->tknzr.lookahead_store + 3){
        consume_new_token(cu, cu->tknzr.lookahead_head);
        cu->tknzr.lookahead_head++;
    }
    *t = cu->tknzr.lookahead_store[2];
}
static inline void peek_nth_token(cunit *cu, token *t, int n){
    while(cu->tknzr.lookahead_head < cu->tknzr.lookahead_store + n){
        consume_new_token(cu, cu->tknzr.lookahead_head);
        cu->tknzr.lookahead_head++;
    }
    *t = cu->tknzr.lookahead_store[n-1];
}
static inline void peek_nth_prefetched_token(cunit *cu, token *t, int a){
    *t = cu->tknzr.lookahead_store[a - 1];
}
static inline void consume_token(cunit* cu, token* t){
    if(cu->tknzr.lookahead_head == cu->tknzr.lookahead_store){
        consume_new_token(cu, t);
    }
    else{
       consume_lookahead_token(cu, t);
    }
}
static inline void clear_lookahead(cunit* cu){
    cu->tknzr.lookahead_head = cu->tknzr.lookahead_store;
}
static inline int get_lookup_count(cunit* cu){
    return (int)(cu->tknzr.lookahead_head - cu->tknzr.lookahead_store);
}

char* store_string(cunit* cu, char* str, char* str_end);
void add_keyword(cunit* cu, const char* str);
//this doesn't respect lookahead, so if you looked ahead, you're gonna get that pos
ureg get_curr_src_line(cunit* cu);
ureg get_curr_src_column(cunit* cu);
char* get_curr_src_file_name(cunit* cu);
void reverse_token(cunit* cu, token* t);
void reverse_until_token(cunit* cu, token* t);
char* get_current_line_string(cunit* cu);
void reverse_to_start_of_line(cunit* cu);