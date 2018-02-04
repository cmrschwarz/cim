#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include <stdio.h>
#include "hms.h"
void hms_dump(hms* h)
{
    printf("Dumping map of length %lu:\n", h->map_end - h->map);
    hms_node* n = h->map;
    while(n != h->map_end){
        if(n->key == NULL){
            printf("----\n");
        }
        else{
            printf("%s --> %s\n", n->key, n->value);
        }
        n++;
    }
}
int main(){

    //printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
    cunit cu;
    cunit_init(&cu);
    for(int i=0;i!=1000000;i++){
         parse_file(&cu, "../test/dummy_src.cim");
    }

    ureg ast_size =  dbuffer_get_size(&cu.ast);
    ureg regs = dbuffer_get_size(&cu.ast) / sizeof(ast_node);

    if(DEBUG_ENUMS){
        printf("ast size(debug): %llu bytes (%llu regs)\n",ast_size,regs);
    }
    else{
        printf("ast size(release): %llu bytes (%llu regs)\n",ast_size,regs);
    }
    printf("string store size: %llu bytes (%llu strings, 19 keywords)\n",
           sbuffer_get_size(&cu.data_store),
           dbuffer_get_size(&cu.string_ptrs) / sizeof(ureg) - 19);

    printf("\noutput:\n");
    //print_ast(&cu);
    cunit_fin(&cu);


    return 0;

}
