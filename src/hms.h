#pragma once
#include "types.h"

//hms: hash map for strings, not His Majesty's Ship
typedef struct  hms_node_t{
    const char* key;
    void* value;
}hms_node;

typedef struct hms_t{
    ureg hash_mask;
    ureg size_bits;
    ureg elem_count;
    ureg grow_on_elem_count;
    hms_node* map;
    hms_node* map_end;
}hms;


ureg hms_hashpos(hms* h, const char* key);
void hms_init(hms* h);
void hms_init_with_n_bits(hms* h, ureg n);
void hms_fin(hms* h);
void hms_set(hms* h, const char* key, void* value);
void* hms_get(hms* h, const char* key);
void* hms_remove(hms* h, const char* key);
void hms_grow(hms* h);
