#include "compiler.h"
#include "parser.h"
#include "ast_printer.h"
void cimc_init(cimc* c, ureg thread_count){
    c->thread_count = thread_count;
    dbuffer_init_with_capacity(&c->files, sizeof(cim_file) * 4);
    ureg parser_size = sizeof(parser) * thread_count;
    dbuffer_init_with_capacity(&c->parsers, parser_size);
    parser* p = dbuffer_claim_space(&c->parsers,  parser_size);
    parser*  pe = p + thread_count;
    while(p != pe){
        parser_init(p);
        p++;
    }
}
void cimc_fin(cimc* c){
    parser* p = (void*)c->parsers.start;
    parser*  pe = p + c->thread_count;
    while(p != pe){
        parser_fin(p);
        p++;
    }
    dbuffer_fin(&c->parsers);
    dbuffer_fin(&c->files);
}
void cimc_add_file(cimc* c, char* path){
    cim_file* f = dbuffer_claim_small_space(&c->files, sizeof(cim_file));
    f->path = path;
    dbuffer_init_with_capacity(&f->ast, sizeof(astn) * 1024);
}
void cimc_compile(cimc* c){
    //no MT yet, srr
    parser* p = (void*)c->parsers.start;
    cim_file* f = (void*)c->files.start;
    cim_file* fe = (void*)c->files.head;
    while(f != fe){
        parse_file(p, f);
        f++;
    }
}
void cimc_dump_asts(cimc* c){
    cim_file* f = (void*)c->files.start;
    cim_file* fe = (void*)c->files.head;
    while(f != fe){
        printf("ast dump of %s: %llu %s nodes -> %llu bytes \n",
               f->path,
               dbuffer_get_size(&f->ast) / sizeof(astn),
               DEBUG_ENUMS ? "debug" : "release",
               dbuffer_get_size(&f->ast));
        print_ast(f);
        f++;
    }
}
void cimc_print_stats(cimc* c){
    //maybe later
}