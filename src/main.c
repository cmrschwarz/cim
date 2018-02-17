#include "compiler.h"

int main(int argc, char **argv)
{
    cimc c;
    cimc_init(&c, 1);
    if(argc == 1) {
        cimc_add_file(&c, "../test/dummy_src.cim");
    }
    else{
        for(int i = 1; i < argc; i++){
            cimc_add_file(&c, argv[i]);
        }
    }
    cimc_compile(&c);
    cimc_dump_asts(&c);
    cimc_fin(&c);
    return 0;
}
