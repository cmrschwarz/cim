#include "scopes.h"
#include "error.h"

int block_scope_insert_type(block_scope* s, bse_type* t){
    bse_type* prev = s->types;
    s->types = t;
    t->prev = prev;
}
se_type* block_scope_lookup_type(block_scope* s, char* name){
    bse_type* t = s->types;
    while(t!=NULL){
        if(t->val.name == name) return &t->val;
    }
    return NULL;
}