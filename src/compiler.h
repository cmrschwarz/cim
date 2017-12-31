#pragma once
#include "dbuffer.h"
#include <assert.h>
#define CIM_ERROR(str)do{printf("%s,\n",str); assert(false);}while(0)
typedef struct cunit_t{
	//later on, this will read the file live, but this is easier for now
	char* str;
	char* pos;
	dbuffer string_store;
	dbuffer string_ptrs;
	//eventually, one ast per function?
	dbuffer ast;
    dbuffer shy_ops;    //shunting yard operators
}cunit;