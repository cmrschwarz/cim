#include "dbuffer.c"
#include "parser.c"
#include <stdio.h>
int main(){
	cunit cu;
	cunit_init(&cu);
	char* str = "int a = 4;";
    printf("%s\n", str);
	cu.str = str;
	cu.pos = str;
	token t;
    cunit_parse(&cu, str);
    cunit_print_ast(&cu);
	return 0;
}
