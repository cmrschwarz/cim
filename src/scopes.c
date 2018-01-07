#include "scopes.h"
#include "error.h"

int block_scope_declare_type(block_scope* s, bse_type* t){
    if(block_scope_lookup_type(s, t->val.name)){
        printf("Redeclaration of the type \"%s\".", t->val.name);
        CIM_ERROR("");
    }
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