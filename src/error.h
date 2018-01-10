#pragma once
#include <stdio.h>
#include <assert.h>
#define DEBUG 1

#define CIM_ERROR(str)do{printf("%s,\n",str); assert(false);}while(0)
#if DEBUG
#   define CIM_ASSERT(cond)assert(cond);
#else
#   define CIM_ASSERT(cond)
#endif