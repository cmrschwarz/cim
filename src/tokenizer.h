#pragma once
#include "compiler.h"
#include "tokens.h"
#include "memory.h"
//TODO: evaluate using token pointers instead
void display_string_store(cunit* cu);
void consume_new_token(cunit* cu, token* t);
static inline void void_lookahead_token(cunit* cu){
    cu->lookahead_head--;
    if(cu->lookahead_head != cu->lookahead_store){
        token* s = cu->lookahead_store;
        do{*s = *(s+1);s++;}while(s != cu->lookahead_head);
    }
}
static inline void consume_lookahead_token(cunit* cu, token* t){
    *t =  cu->lookahead_store[0];
    void_lookahead_token(cu);
}
static inline void peek_token(cunit *cu, token *t){
    if(cu->lookahead_head == cu->lookahead_store){
        consume_new_token(cu, cu->lookahead_head);
        cu->lookahead_head++;
    }
    *t = *cu->lookahead_store;
}
static inline void peek_2nd_token(cunit *cu, token *t){
    while(cu->lookahead_head < cu->lookahead_store + 2){
        consume_new_token(cu, cu->lookahead_head);
        cu->lookahead_head++;
    }
    *t = cu->lookahead_store[1];
}
static inline void peek_3rd_token(cunit *cu, token *t){
    while(cu->lookahead_head < cu->lookahead_store + 3){
        consume_new_token(cu, cu->lookahead_head);
        cu->lookahead_head++;
    }
    *t = cu->lookahead_store[2];
}
static inline void peek_nth_token(cunit *cu, token *t, int n){
    while(cu->lookahead_head < cu->lookahead_store + n){
        consume_new_token(cu, cu->lookahead_head);
        cu->lookahead_head++;
    }
    *t = cu->lookahead_store[n-1];
}
static inline void peek_nth_prefetched_token(cunit *cu, token *t, int a){
    *t = cu->lookahead_store[a - 1];
}
static inline void consume_token(cunit* cu, token* t){
    if(cu->lookahead_head == cu->lookahead_store){
        consume_new_token(cu, t);
    }
    else{
       consume_lookahead_token(cu, t);
    }
}
static inline void clear_lookahead(cunit* cu){
    cu->lookahead_head = cu->lookahead_store;
}
static inline int get_lookup_count(cunit* cu){
    return (int)(cu->lookahead_head - cu->lookahead_store);
}

char* store_string(cunit* cu, char* str, char* str_end);
void add_keyword(cunit* cu, const char* str);


