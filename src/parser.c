#include "parser.h"
#include "tokenizer.h"
#include <stdio.h>
#include <memory.h>
#include "ast.h"
#include <assert.h>

#define OP_RANGE 255
static u8 prec_table [OP_RANGE];
static u8 assoc_table [OP_RANGE];
enum assocs{
    LEFT_ASSOCIATIVE = 0,
    RIGHT_ASSOCIATIVE = 1,
};
void init(cunit* cu){
    if(prec_table[OP_NONE] == 0){
        prec_table[OP_NONE] = 1; //initialization flag
        prec_table[OP_TEMP_PAREN_OPEN] = 0;   // '(' uniquely has this precedence level so it is not removed during flush
        prec_table[OP_ADD] = 1;
        prec_table[OP_SUBTRACT] = 1;
        prec_table[OP_PREINCREMENT] = 99;
        prec_table [OP_POSTINCREMENT] =9;
        prec_table[OP_DIVIDE] = 3;
        prec_table[OP_MULTIPLY] = 3;
    }
    if(assoc_table[OP_NONE] == 0){
        assoc_table[OP_NONE] = 1;
    }
	dbuffer_init(&cu->string_store);
	dbuffer_init(&cu->string_ptrs);
	dbuffer_init(&cu->ast);
    dbuffer_init(&cu->shy_ops);
}

typedef struct shy_op_t{
    u8 type;
    u8 op;
}shy_op;
static void parse_meta(cunit* cu, token* t1){
    
}
static inline void flush_shy_op(cunit* cu, shy_op* s, ureg expr_start){
    expr_elem* expr_rend = (void*)(cu->ast.start + expr_start + sizeof(expr_elem) - sizeof(expr_elem));
    expr_elem* expr_rit =  (void*)(cu->ast.head - sizeof(expr_elem));
    int its;
    switch (s->type){
        case EXPR_ELEM_TYPE_BRACE: its = 0;break;
        case EXPR_ELEM_TYPE_OP_LR: its = 2;break;
        default: its = 1; break;
    }
    for(int i = 0; i != its; i++){
        if(expr_rit->type == EXPR_ELEM_TYPE_NUMBER){
            expr_rit--;
        }
        else{
            expr_rit = (void*)cu->ast.start + expr_rit->val;
        }
    }
    expr_elem* e = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
    e->type = s->type;
    e->op = s->op;
    e->val = (u8*)expr_rit - cu->ast.start;
    cu->shy_ops.head -= sizeof(shy_op); //this will be inlined and brought out of the loop
}
static inline void push_shy_op(cunit* cu, shy_op* sop, shy_op** sho_ri, shy_op** sho_re, ureg shy_ops_start){
    if(dbuffer_has_space(&cu->shy_ops, sizeof(shy_op))){
        *((shy_op*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(shy_op);
        (*sho_ri)++;
    }
    else{
        dbuffer_grow(&cu->shy_ops);
        *((shy_op*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(shy_op);
        *sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(shy_op));
        *sho_ri = (void*)(cu->shy_ops.head - sizeof(shy_op));
    }
}
static int parse_expr(cunit *cu, token_type term1, token_type term2, token *t1, token *t2, bool sub_expr){
    //printf("parsing expr (%llu)\n", POS(cu->ast.head));
    if(t2->type == term1 || t2->type == term2){
        //short expression optimization
        if(t1->type == TOKEN_NUMBER){
            expr_elem* n = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
            n->type = ASTNT_NUMBER;
            n->val = t1->str;
        }
		if(t1->type == TOKEN_STRING){
            expr_elem* v = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
            v->type = ASTNT_VARIABLE;
            v->val = t1->str;
        }
        return (t2->type == term1) ? 0 : 1;
    }
    ureg expr_start;
    expr_elem* expr;
    if(!sub_expr){
        expr = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
        expr->type = ASTNT_EXPRESSION;
        expr_start = (u8*)expr - cu->ast.start;
    }
    else{
        expr_start = dbuffer_get_size(&cu->ast) - sizeof(expr_elem);
    }
    expr_elem* e;
    shy_op sop;
    bool second_available = true;
    ureg shy_ops_start = cu->shy_ops.head - cu->shy_ops.start;
    bool expecting_op = false;
    u8 prec;
    shy_op* sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(shy_op));
    shy_op* sho_ri = (void*)(cu->shy_ops.head - sizeof(shy_op));
    while(true){
        if(t1->type == term1 || t1->type == term2){
            for(;sho_ri != sho_re; sho_ri--){
                flush_shy_op(cu, sho_ri, expr_start);
            }
            if(!sub_expr){
                expr = (expr_elem*)(cu->ast.start + expr_start);
                expr->val = dbuffer_get_size(&cu->ast);
            }
            else{
                expr = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
                expr->type = ASTNT_EXPRESSION;
                expr->val = expr_start;
            }
            return (t1->type == term1) ? 0 : 1;
        }
        switch(t1->type){
            case TOKEN_DOUBLE_PLUS: {
                sop.op = (expecting_op) ? OP_POSTINCREMENT : OP_PREINCREMENT;
            }goto lbl_op_l_or_r;
            case TOKEN_DOUBLE_MINUS:{
                sop.op = (expecting_op) ? OP_POSTDECREMENT : OP_PREDECREMENT;
            }//fallthrough to op_l_or_r
            lbl_op_l_or_r:{
                sop.type = (expecting_op) ? EXPR_ELEM_TYPE_OP_R : EXPR_ELEM_TYPE_OP_L;
                prec = prec_table[sop.op];
                for (; sho_ri != sho_re && prec_table[sho_ri->op] > prec; sho_ri--) {
                    flush_shy_op(cu, sho_ri, expr_start);
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                //expecting op stays the same
            }break;
            case TOKEN_STAR:{
                if (expecting_op) {
                    sop.op = OP_MULTIPLY;
                    goto lbl_op_lr;
                }
                sop.op = OP_DEREFERENCE;
            }goto lbl_op_unary;
            case TOKEN_PLUS: {
                if (expecting_op) {
                    sop.op = OP_ADD;
                    goto lbl_op_lr;
                }
                sop.op = OP_UNARY_PLUS;
            }goto lbl_op_unary;
            case TOKEN_MINUS:{
                if (expecting_op) {
                    sop.op = OP_SUBTRACT;
                    goto lbl_op_lr;
                }
                sop.op = OP_UNARY_MINUS;
            }//fallthrough to lbl_op_unary
            lbl_op_unary: {
                sop.type = EXPR_ELEM_TYPE_UNARY;
                prec = prec_table[sop.op];
                for (; sho_ri != sho_re && prec_table[sho_ri->op] > prec; sho_ri--) {
                    flush_shy_op(cu, sho_ri, expr_start);
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                //expecting_op is already false, otherwise it wouldn't be unary
            } break;
            case TOKEN_SLASH:
                sop.op = OP_DIVIDE;
                goto lbl_op_lr;
            case TOKEN_SLASH_EQUALS:
                sop.op = OP_DIVIDE_ASSIGN;
                goto lbl_op_lr;
            case TOKEN_STAR_EQUALS:
                sop.op = OP_MULTIPLY_ASSIGN;
                goto lbl_op_lr;
            lbl_op_lr: {
                sop.type = EXPR_ELEM_TYPE_OP_LR;
                prec = prec_table[sop.op];
                if (assoc_table[sop.op] == LEFT_ASSOCIATIVE) {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op] >= prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri, expr_start);
                    }
                } else {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op] > prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri, expr_start);
                    }
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_OPEN: {
                sop.type = EXPR_ELEM_TYPE_BRACE;
                sop.op = OP_TEMP_PAREN_OPEN;
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_CLOSE: {
                for (;sho_ri != sho_re && sho_ri->op != OP_TEMP_PAREN_OPEN;
                      sho_ri--)
                {
                    flush_shy_op(cu, sho_ri, expr_start);
                }
                //removing the OP_TEMP_PAREN_OPEN
                dbuffer_pop_back(&cu->shy_ops, sizeof(shy_op));
                sho_ri--;
                expecting_op = true;
            }break;
            case TOKEN_NUMBER: {
                assert(!expecting_op);
                e = dbuffer_claim_small_space(&cu->ast, sizeof(*e));
                e->type = EXPR_ELEM_TYPE_NUMBER;
                e->val = t1->str;
                expecting_op = true;
            }break;
            case TOKEN_STRING: {
                assert(!expecting_op);
                if (!second_available) {
                    get_token(cu, t2);
                    second_available = true;
                }
                if (t2->type == TOKEN_PAREN_OPEN) {
                    ureg fn_name = t1->str;
                    ureg fn_end = dbuffer_get_size(&cu->ast) - sizeof(expr_elem);
                    do {
                        get_token(cu, t1);
                        get_token(cu, t2);
                    } while (parse_expr(cu, TOKEN_COMMA, TOKEN_PAREN_CLOSE, t1, t2, true) == 0);
                    e = dbuffer_claim_small_space(&cu->ast, 2 * sizeof(*e));
                    e->type = EXPR_ELEM_TYPE_FN_NAME;
                    e->val = fn_name;
                    e++;
                    e->type = EXPR_ELEM_TYPE_FN_CALL;
                    e->val = fn_end;
                    second_available = false;
                } else {
                    e = dbuffer_claim_small_space(&cu->ast, sizeof(*e));
                    e->type = EXPR_ELEM_TYPE_VARIABLE;
                    e->val = t1->str;
                }
                expecting_op = true;
            }break;
            case TOKEN_EOF:{
                printf("Unexpected eof.");
            }exit(-1);
            default:{
                 printf("Unexpected token.");
            }exit(-1);
        }
        if(!second_available){
            get_token(cu, t1);
        }
        else {
            second_available = false;
            *t1 = *t2;
        }
    }
}
static int parse_normal_declaration(cunit* cu, token* t1, token* t2){
    astn_declaration* d =
     dbuffer_claim_small_space(&cu->ast, sizeof(astn_declaration));
    d->astnt = ASTNT_DECLARATION;
    d->type = t1->str;
    d->name = t2->str;
    get_token(cu, t1);
    if(t1->type == TOKEN_SEMICOLON){
        d->assigning = false;
    }
    else if(t1->type == TOKEN_EQUALS){
        d->assigning = true;
        get_token(cu, t1);
        get_token(cu, t2);
        return parse_expr(cu, TOKEN_SEMICOLON,TOKEN_SEMICOLON, t1, t2, false);
    }
    else{
        printf("Unexpected Token\n");
        exit(-1);
    }
    return 0;
    printf("parsed declaration\n"); 
}
static int parse_next(cunit* cu){
    token t1;
    token t2;
    get_token(cu, &t1);
    switch (t1.type){
        case TOKEN_STRING:
            get_token(cu, &t2);
            if (t2.type == TOKEN_STRING)
                return parse_normal_declaration(cu, &t1, &t2);
            else return parse_expr(cu, ';', ';', &t1, &t2, false);
        case TOKEN_NUMBER:
            get_token(cu, &t2);
            return parse_expr(cu, ';',';', &t1, &t2, false);
        case TOKEN_HASH:
        case TOKEN_DOUBLE_HASH:
            parse_meta(cu, &t1);
            break;
        case TOKEN_EOF:
            return -1;
        default:
            printf("parse_next: unexpected token\n");
            return -1;
    }
    return 0;
}
void parse(cunit* cu, char* str){
    cu->str = str;
    /*
    token t;
	get_token(cu, &t);
	while(t.type != TOKEN_TYPE_EOF){
		print_token(cu, &t);
		printf(" (%i)", t.rel_str);
		putchar('\n');
		get_token(cu, &t);
	}	
    */
    // actual parsing
    while(parse_next(cu)!=-1); 
}
