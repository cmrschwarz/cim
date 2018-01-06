#include "scopes.h"
#include "error.h"

int block_scope_declare_type(block_scope* s, se_type* t){
    if(block_scope_lookup_type(s, t->name)){
        printf("Redeclaration of the type \"%s\".", t->name);
        CIM_ERROR("");
    }
    bse_type** tlist_end = s->types;
    while(*tlist_end != NULL)tlist_end  = &(*tlist_end)->next;
    *tlist_end = t;
}
se_type* block_scope_lookup_type(block_scope* s, char* name){
    bse_type* t = s->types;
    while(t!=NULL){
        if(t->val.name == name) return &t->val;
    }
    return NULL;
}