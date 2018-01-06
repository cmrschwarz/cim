#pragma once
#include <stdio.h>
#include <assert.h>
#define CIM_ERROR(str)do{printf("%s,\n",str); assert(false);}while(0)