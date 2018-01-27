#pragma once
#include <stdio.h>
#include <assert.h>
#include <signal.h>
#define DEBUG 1


#if DEBUG
#   define CIM_EXIT raise(SIGTRAP)
#   define CIM_ASSERT(cond)assert(cond);
#else
#   define CIM_EXIT exit(-1)
#   define CIM_ASSERT(cond)
#endif
#   define CIM_ERROR(str)do{printf("%s,\n",str); CIM_EXIT;}while(0)