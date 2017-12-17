#include "dbuffer.c"
#include "parser.c"
#include <stdio.h>
int main(){
	compilation_unit cu;
	compilation_unit_init(&cu);
	char* str = "int a = 4";
	cu.str = str;
	cu.pos = str;
	token t;
	compilation_unit_get_token(&cu, &t);
	while(t.type != TOKEN_TYPE_EOF){
		compilation_unit_print_token(&cu, &t);
		printf(" (%i)", t.str_rel);
		putchar('\n');
		compilation_unit_get_token(&cu, &t);
	}	
	printf("Hello gcc, my love");
	return 0;
}
