#pragma once
#include "types.h"

typedef struct dbuffer {
    u8* start;
    u8* end;  // [start; end[
    u8* head;
} dbuffer;

void dbuffer_init_with_capacity(dbuffer* db, ureg size);
void dbuffer_init(dbuffer* db);
void dbuffer_free(dbuffer* db);

ureg dbuffer_get_capacity(dbuffer* db);
ureg dbuffer_get_size(dbuffer* db);

void dbuffer_get_from_idx(dbuffer* db, void* target, ureg pos, ureg size);
void dbuffer_get(dbuffer* db, void* target, void* pos, ureg size);

bool dbuffer_is_emtpy(dbuffer* db);
void dbuffer_set_capacity(dbuffer* db, ureg new_size);
void dbuffer_set_bigger_capacity(dbuffer* db, ureg new_size);
void dbuffer_grow(dbuffer* db);
int dbuffer_has_space(dbuffer* db, ureg required_space);
void dbuffer_make_space(dbuffer* db, ureg required_space);
void dbuffer_make_small_space(dbuffer* db, ureg required_space);

void* dbuffer_claim_space(dbuffer* db, ureg required_space);
void* dbuffer_claim_small_space(dbuffer* db, ureg required_space);

void dbuffer_remove_at(dbuffer* db, void* pos, ureg size);
void dbuffer_insert_at(dbuffer* db, const void* data, void* pos, ureg size);
void dbuffer_swap(dbuffer* db, void* posa, void* posb, ureg size);

void dbuffer_pop_back(dbuffer* db, ureg size);
void dbuffer_push_back(dbuffer* db, const void* data, ureg size);

#define dbuffer_push_back_val(db, val)\
     do{(*(typeof(val) *)dbuffer_claim_space(db, sizeof(val))) = (val);}while(0)
