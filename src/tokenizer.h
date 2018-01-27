#pragma once
#include "compiler.h"
#include "tokens.h"
#include "memory.h"
//TODO: evaluate using token pointers instead
char* store_string(cunit* cu, char* str, char* str_end);
void add_keyword(cunit* cu, const char* str);
ureg get_curr_src_line(cunit* cu);
ureg get_curr_src_column(cunit* cu);
char* get_curr_src_file_name(cunit* cu);
ureg get_column_of_token_curr_minus_n(cunit* cu, ureg n);

void get_token_range(cunit* cu, ureg tbase_column_start, int offs, ureg cnt,
                     ureg* start, ureg* end);
void error(cunit* cu, int offs, int cnt, char* str, ...);

const char* get_token_type_str(token_type t);
const char* make_token_string(cunit*cu, token* t);
void tokenizer_open_file(cunit* cu, char* filename);
void tokenizer_close_file(cunit* cu);
void display_string_store(cunit* cu);
void consume_new_token(cunit* cu, token* t);
static inline int get_lookahead_count(cunit* cu){
    return cu->tknzr.lookahead_count;

}
static inline void set_line_backup(cunit* cu, looahead_line_backup* lb){
    lb->line_number = cu->tknzr.line_number;
    lb->line_column = get_curr_src_column(cu);
    lb->line_comment_offset = cu->tknzr.line_comment_offset;
}
static inline void void_lookahead_token(cunit* cu){
    cu->tknzr.lookahead_count--;
    if(cu->tknzr.lookahead_count != 0){
        token* s = cu->tknzr.lookahead_store;
        token* e = cu->tknzr.lookahead_store + cu->tknzr.lookahead_count;
        do{*s = *(s+1);s++;}while(s != e);
        looahead_line_backup* sl = cu->tknzr.line_backup;
        looahead_line_backup* el = cu->tknzr.line_backup + cu->tknzr.lookahead_count;
        do{*sl = *(sl+1);sl++;}while(sl != el);
    }
}
static inline void void_2_lookahead_tokens(cunit* cu){
    cu->tknzr.lookahead_count-=2;
    if(cu->tknzr.lookahead_count != 0){
        token* s = cu->tknzr.lookahead_store;
        token* e = cu->tknzr.lookahead_store + cu->tknzr.lookahead_count;
        do{*s = *(s+2);s++;}while(s != e);
        looahead_line_backup* sl = cu->tknzr.line_backup;
        looahead_line_backup* el = cu->tknzr.line_backup + cu->tknzr.lookahead_count;
        do{*sl = *(sl+1);sl++;}while(sl != el);
    }
}
static inline void consume_lookahead_token(cunit* cu, token* t){
    *t =  cu->tknzr.lookahead_store[0];
    void_lookahead_token(cu);
}
static inline void peek_token(cunit *cu, token *t){
    if(cu->tknzr.lookahead_count == 0){
        cu->lah++;
        set_line_backup(cu, cu->tknzr.line_backup);
        consume_new_token(cu, cu->tknzr.lookahead_store);
        cu->tknzr.lookahead_count=1;
    }
    *t = *cu->tknzr.lookahead_store;
}
static inline void peek_2nd_token(cunit *cu, token *t){
    while(cu->tknzr.lookahead_count < 2){
         cu->lah++;
        set_line_backup(cu, cu->tknzr.line_backup + cu->tknzr.lookahead_count);
        consume_new_token(cu, cu->tknzr.lookahead_store + cu->tknzr.lookahead_count);
        cu->tknzr.lookahead_count++;
    }
    *t = cu->tknzr.lookahead_store[1];
}
static inline void peek_nth_token(cunit *cu, token *t, int n){
    while(cu->tknzr.lookahead_count < n){
         cu->lah++;
        set_line_backup(cu, cu->tknzr.line_backup + cu->tknzr.lookahead_count);
        consume_new_token(cu, cu->tknzr.lookahead_store + cu->tknzr.lookahead_count);
        cu->tknzr.lookahead_count++;
    }
    *t = cu->tknzr.lookahead_store[n-1];
}
static inline void peek_nth_prefetched_token(cunit *cu, token *t, int a){
    *t = cu->tknzr.lookahead_store[a - 1];
}
static inline void consume_token(cunit* cu, token* t){
    if(cu->tknzr.lookahead_count == 0){
        cu->cons++;
        consume_new_token(cu, t);
    }
    else{
        consume_lookahead_token(cu, t);
    }
}
static inline void clear_lookahead(cunit* cu){
    cu->tknzr.lookahead_count = 0;
}
