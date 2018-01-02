#pragma once
#include "dbuffer.h"

typedef struct nsp_scope_s{
    dbuffer types;
    dbuffer vars;
    dbuffer functions;
}nsp_scope;