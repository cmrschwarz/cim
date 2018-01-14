#pragma once
#include "dbuffer.h"
#include "sbuffer.h"
#include <assert.h>
#include "token.h"

typedef struct cunit_t{
	//later on, this will read the file live, but this is easier for now
    char* str;
	char* pos;
	sbuffer data_store;
	dbuffer string_ptrs;
	dbuffer ast;
    dbuffer shy_ops;    //shunting yard operators
    token lookahead_store[3]; //no more is ever needed
    token* lookahead_head;
}cunit;
