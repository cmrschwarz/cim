#include "parser.h"
#include <stdio.h>
#include "ast.h"
#include <assert.h>
#include "error.h"
#include "keywords.h"
#include "compiler.h"

static inline int parse_elem(cunit* cu);
static int parse_expr(cunit *cu, token_type term1, token_type term2, bool sub_expr);
static ast_node* parse_type(cunit* cu);

#define OP_RANGE (1 << (sizeof(u8)*8))
static u8 prec_table[OP_RANGE];
static u8 assoc_table[OP_RANGE];

enum assocs{
    LEFT_ASSOCIATIVE = 0,
    RIGHT_ASSOCIATIVE = 1,
};

static u8 prec_table [OP_RANGE] = {
    //uniquely has this precedence level so it is not removed during flush
    [OP_TEMP_PAREN_OPEN] = 0,

    [OP_POSTINCREMENT] = 15,
    [OP_POSTDECREMENT] = 15,

    [OP_PREINCREMENT] = 14,
    [OP_PREDECREMENT] = 14,
    [OP_LOGICAL_NOT] = 14,
    [OP_BITWISE_NOT] = 14,
    [OP_DEREFERENCE] = 14,
    [OP_ADDRESS_OF] = 14,

    [OP_MULTIPLY] = 13,
    [OP_DIVIDE] = 13,
    [OP_MODULO] = 13,

    [OP_ADD] = 12,
    [OP_SUBTRACT] = 12,

    [OP_LSHIFT] = 11,
    [OP_RSHIFT] = 11,

    [OP_LESS_THAN] = 10,
    [OP_GREATER_THAN] = 10,
    [OP_LESS_THAN_OR_EQUAL] = 10,
    [OP_GREATER_THAN_OR_EQUAL] = 10,

    [OP_EQUALS] = 9,
    [OP_NOT_EQUAL] = 9,

    [OP_BITWISE_AND] = 8,

    [OP_BITWISE_XOR] = 7,

    [OP_BITWISE_OR] = 6,

    [OP_LOGICAL_AND] = 5,

    [OP_LOGICAL_XOR] = 4,

    [OP_LOGICAL_OR] = 3,

    [OP_ASSIGN] = 2,
    [OP_ADD_ASSIGN] = 2,
    [OP_SUBTRACT_ASSIGN] = 2,
    [OP_MULTIPLY_ASSIGN] = 2,
    [OP_DIVIDE_ASSIGN] = 2,
    [OP_MODULO_ASSIGN] = 2,
    [OP_LSHIFT_ASSIGN] = 2,
    [OP_RSHIFT_ASSIGN] = 2,
    [OP_BITWISE_AND_ASSIGN] = 2,
    [OP_BITWISE_OR_ASSIGN] = 2,
    [OP_BITWISE_XOR_ASSIGN] = 2,
    [OP_BITWISE_NOT_ASSIGN] = 2,
};

static u8 assoc_table [OP_RANGE] = {
    [OP_PREINCREMENT] = RIGHT_ASSOCIATIVE,
    [OP_PREDECREMENT] = RIGHT_ASSOCIATIVE,
    [OP_LOGICAL_NOT] = RIGHT_ASSOCIATIVE,
    [OP_BITWISE_NOT] = RIGHT_ASSOCIATIVE,
    [OP_DEREFERENCE] = RIGHT_ASSOCIATIVE,
    [OP_ADDRESS_OF] = RIGHT_ASSOCIATIVE,
    [OP_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_ADD_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_SUBTRACT_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_MULTIPLY_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_DIVIDE_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_MODULO_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_LSHIFT_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_RSHIFT_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_BITWISE_AND_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_BITWISE_OR_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_BITWISE_XOR_ASSIGN] = RIGHT_ASSOCIATIVE,
    [OP_BITWISE_NOT_ASSIGN] = RIGHT_ASSOCIATIVE,
};

void cunit_init(cunit* cu){
	sbuffer_init(&cu->data_store, 4);
	dbuffer_init(&cu->string_ptrs);
	dbuffer_init(&cu->ast);
    dbuffer_init_with_capacity(&cu->parsr.shy_ops, 1000);
    dbuffer_init_with_capacity(&cu->tknzr.file_buffer, sizeof(ureg) * 128);
    cu->tknzr.file = NULL;
    clear_lookahead(cu);



    add_keyword(cu, KEYWORD_IF);
    add_keyword(cu, KEYWORD_ELSE);
    add_keyword(cu, KEYWORD_BREAK);
    add_keyword(cu, KEYWORD_RETURN);

    add_keyword(cu, KEYWORD_SWITCH);
    add_keyword(cu, KEYWORD_CASE);

    add_keyword(cu, KEYWORD_WHILE);
    add_keyword(cu, KEYWORD_FOR);
    add_keyword(cu, KEYWORD_STRUCT);
    add_keyword(cu, KEYWORD_ENUM);
    add_keyword(cu, KEYWORD_UNION);

    add_keyword(cu, KEYWORD_CONTINUE);
    add_keyword(cu, KEYWORD_INLINE);
    add_keyword(cu, KEYWORD_CONST);
    add_keyword(cu, KEYWORD_CAST);
    add_keyword(cu, KEYWORD_STATIC);
    add_keyword(cu, KEYWORD_TYPEDEF);
    add_keyword(cu, KEYWORD_LABEL);
    add_keyword(cu, KEYWORD_GOTO);
}
void cunit_fin(cunit* cu) {
    sbuffer_fin(&cu->data_store);
    dbuffer_fin(&cu->string_ptrs);
    dbuffer_fin(&cu->ast);
    dbuffer_fin(&cu->parsr.shy_ops);
}
static inline ureg get_ast_size(cunit* cu){
    return dbuffer_get_size(&cu->ast);
}
static inline void* claim_ast_space(cunit* cu, ureg size){
    return dbuffer_claim_small_space(&cu->ast, size);
}
static inline ast_rel_ptr get_ast_growth(cunit* cu, ureg ast_start){
    return (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - ast_start) / sizeof(ast_node));
}
static inline void add_size_node(cunit* cu, ureg ast_start){
    ast_node* n = claim_ast_space(cu, sizeof(ast_node));
    n->expr.size = get_ast_growth(cu, ast_start);
}
static inline void consume_required_token(cunit* cu, token_type t){
    token t1;
    consume_token(cu, &t1);
    CIM_ASSERT(t1.type == t);
}
static inline void flush_shy_op(cunit* cu, ast_node* s){
    ast_node* expr_rit =  (void*)(cu->ast.head - sizeof(ast_node));
    int its;
    switch (s->expr.type){
        case EXPR_NODE_PAREN: its = 0;break;
        case EXPR_NODE_OP_LR: its = 2;break;
        default: its = 1; break;
    }
    for(int i = 0; i != its; i++){
        expr_rit = expr_rit - expr_rit->expr.size;
    }
    //needs to be precomputed because the realloc might invalidate the expr_rit ptr
    ast_rel_ptr size = (ast_rel_ptr)((ast_node*)cu->ast.head - expr_rit);
    ast_node* e = claim_ast_space(cu, sizeof(ast_node));
    //shy ops don't come from the ast but from shy_ops therefore s doesn't get invalidated
    e->expr.type = s->expr.type;
    e->expr.special.opcode = s->expr.special.opcode;
    e->expr.size = size;
    //this will hopefully be inlined and brought out of the loop
    cu->parsr.shy_ops.head -= sizeof(ast_node);
}
static inline void push_shy_op(cunit* cu, ast_node* sop, ast_node** sho_root, ast_node** sho_ri,
                               ast_node** sho_re){
    if(dbuffer_has_space(&cu->parsr.shy_ops, sizeof(ast_node))){
        *((ast_node*)cu->parsr.shy_ops.head) = *sop;
        cu->parsr.shy_ops.head += sizeof(ast_node);
        (*sho_ri)++;
    }
    else{
        dbuffer_grow(&cu->parsr.shy_ops);
        *((ast_node*)cu->parsr.shy_ops.head) = *sop;
        cu->parsr.shy_ops.head += sizeof(ast_node);
        *sho_re = (void*)(cu->parsr.shy_ops.start + (*sho_re - *sho_root) * sizeof(ast_node));
        *sho_ri = (void*)(cu->parsr.shy_ops.head - sizeof(ast_node));
        *sho_root = (void*)(cu->parsr.shy_ops.start);
    }
}
static inline void recalc_sho_its(cunit* cu, ast_node** sho_root, ast_node** sho_ri,
                                  ast_node** sho_re)
{
    if((void*)(*sho_re) != cu->parsr.shy_ops.start){
        ureg sho_ri_new = *sho_ri - *sho_root;
        ureg sho_re_new = *sho_re - *sho_root;
        *sho_root = (void*)cu->parsr.shy_ops.start;
        *sho_ri = *sho_root + sho_ri_new;
        *sho_re = *sho_root + sho_re_new;
    }
}
static inline ast_rel_ptr parse_arg_list(cunit* cu, token_type end_tok){
    ureg ast_pos = get_ast_size(cu);
    token t;
    peek_token(cu, &t);
    if(t.type == end_tok){
        void_lookahead_token(cu);
        return 0;
    }
    while (parse_expr(cu, TOKEN_COMMA, end_tok, true) == 0);
    return get_ast_growth(cu, ast_pos);
}
static inline ast_rel_ptr parse_param_list(cunit* cu, token_type end_tok){
    token t1;
    peek_token(cu, &t1);
    if(t1.type != end_tok){
        do{
            parse_type(cu);
            ast_node* n = claim_ast_space(cu, sizeof(ast_node));
            consume_token(cu, &t1);
            CIM_ASSERT(t1.type == TOKEN_STRING);
            n->str = t1.str;
            consume_token(cu, &t1);
        }while(t1.type == TOKEN_COMMA);
        CIM_ASSERT(t1.type == end_tok);
    }
    else{
        void_lookahead_token(cu);
    }
}
static int continue_parse_expr(cunit* cu, token_type term1, token_type term2, bool sub_expr,
                               ureg expr_start, ureg shy_ops_start,ureg shy_op_pos,
                               bool expecting_op)
{
    token t1;
    token t2;
    ast_node* e;
    ast_node sop;
    u8 prec;
    ast_node* sho_root = (void*)(cu->parsr.shy_ops.start);
    ast_node* sho_re = (ast_node*)(cu->parsr.shy_ops.start + shy_ops_start) - 1;
    ast_node* sho_ri = (ast_node*)(cu->parsr.shy_ops.start + shy_op_pos) - 1;
    ureg open_paren_count = 0;
    consume_token(cu, &t1);
    while(true){
        switch(t1.type){
            case TOKEN_DOUBLE_PLUS: {
                sop.expr.special.opcode= (expecting_op) ? OP_POSTINCREMENT : OP_PREINCREMENT;
            }goto lbl_op_l_or_r;
            case TOKEN_DOUBLE_MINUS:{
                sop.expr.special.opcode= (expecting_op) ? OP_POSTDECREMENT : OP_PREDECREMENT;
            }//fallthrough to op_l_or_r
            lbl_op_l_or_r:{
                sop.expr.type = (expecting_op) ? EXPR_NODE_OP_R : EXPR_NODE_OP_L;
                prec = prec_table[sop.expr.special.opcode];
                if (assoc_table[sop.expr.special.opcode] == LEFT_ASSOCIATIVE) {
                    for (; sho_ri != sho_re && prec_table[sho_ri->expr.special.opcode] >= prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                else {
                    for (; sho_ri != sho_re && prec_table[sho_ri->expr.special.opcode] > prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                push_shy_op(cu, &sop, &sho_root, &sho_ri, &sho_re);
                //expecting op stays the same
            }break;
            case TOKEN_STAR:{
                if (expecting_op) {
                    sop.expr.special.opcode= OP_MULTIPLY;
                    goto lbl_op_lr;
                }
                sop.expr.special.opcode= OP_DEREFERENCE;
            }goto lbl_op_unary;
            case TOKEN_AND:{
                if (expecting_op) {
                    sop.expr.special.opcode= OP_BITWISE_AND;
                    goto lbl_op_lr;
                }
                sop.expr.special.opcode= OP_ADDRESS_OF;
            }goto lbl_op_unary;
            case TOKEN_PLUS: {
                if (expecting_op) {
                    sop.expr.special.opcode= OP_ADD;
                    goto lbl_op_lr;
                }
                sop.expr.special.opcode= OP_UNARY_PLUS;
            }goto lbl_op_unary;
            case TOKEN_MINUS:{
                if (expecting_op) {
                    sop.expr.special.opcode= OP_SUBTRACT;
                    goto lbl_op_lr;
                }
                sop.expr.special.opcode= OP_UNARY_MINUS;
            }//fallthrough to lbl_op_unary
            lbl_op_unary: {
                sop.expr.type = EXPR_NODE_OP_L;
                prec = prec_table[sop.expr.special.opcode];
                //unary is always right associative
                for (; sho_ri != sho_re && prec_table[sho_ri->expr.special.opcode] > prec; sho_ri--) {
                    flush_shy_op(cu, sho_ri);
                }
                push_shy_op(cu, &sop, &sho_root, &sho_ri, &sho_re);
                //expecting_op is already false, otherwise it wouldn't be unary
            } break;
            case TOKEN_SLASH:
            case TOKEN_SLASH_EQUALS:
            case TOKEN_STAR_EQUALS:
            case TOKEN_LESS_THAN:
            case TOKEN_LESS_THAN_EQUALS:
            case TOKEN_GREATER_THAN:
            case TOKEN_GREATER_THAN_EQUALS:
            case TOKEN_DOUBLE_LESS_THAN:
            case TOKEN_DOUBLE_GREATER_THAN:
            case TOKEN_EQUALS:
            case TOKEN_DOUBLE_EQUALS:
            case TOKEN_EXCLAMATION_MARK:
            case TOKEN_EXCLAMATION_MARK_EQUALS:
            case TOKEN_MINUS_EQUALS:
            case TOKEN_PLUS_EQUALS:
            case TOKEN_PERCENT:
            case TOKEN_PERCENT_EQUALS:
            case TOKEN_DOUBLE_GREATER_THAN_EQUALS:
            case TOKEN_DOUBLE_AND:
            case TOKEN_CARET:
            case TOKEN_CARET_EQUALS:
            case TOKEN_DOUBLE_CARET:
            case TOKEN_PIPE:
            case TOKEN_PIPE_EQUALS:
            case TOKEN_DOUBLE_PIPE:

            case TOKEN_TILDE:
            case TOKEN_TILDE_EQUALS:
            case TOKEN_DOUBLE_LESS_THAN_EQUALS:{
                //for these, the toke  type is set to be equal to the op type
                sop.expr.special.opcode= (u8)(t1.type);
            }//fall through to op_lr
            lbl_op_lr: {
                sop.expr.type= EXPR_NODE_OP_LR;
                prec = prec_table[sop.expr.special.opcode];
                if (assoc_table[sop.expr.special.opcode] == LEFT_ASSOCIATIVE) {
                    for (; sho_ri != sho_re && prec_table[sho_ri->expr.special.opcode] >= prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                else {
                    for (; sho_ri != sho_re && prec_table[sho_ri->expr.special.opcode] > prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                push_shy_op(cu, &sop, &sho_root, &sho_ri, &sho_re);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_OPEN: {
                open_paren_count++;
                sop.expr.type = EXPR_NODE_PAREN;
                sop.expr.special.opcode= OP_TEMP_PAREN_OPEN;
                push_shy_op(cu, &sop, &sho_root, &sho_ri, &sho_re);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_CLOSE: {
                if(open_paren_count==0)goto lbl_default;
                open_paren_count--;
                for (;sho_ri != sho_re && sho_ri->expr.special.opcode!= OP_TEMP_PAREN_OPEN;
                      sho_ri--)
                {
                    flush_shy_op(cu, sho_ri);
                }
                //removing the OP_TEMP_PAREN_OPEN
                dbuffer_pop_back(&cu->parsr.shy_ops, sizeof(ast_node));
                sho_ri--;
                expecting_op = true;
            }break;
            case TOKEN_NUMBER:
            case TOKEN_LITERAL:
            case TOKEN_BINARY_LITERAL:
            {
                assert(!expecting_op);
                e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->str = t1.str;
                e++;
                e->expr.size = 2;
                e->expr.type= (expr_node_type)t1.type;
                expecting_op = true;
            }break;
            case TOKEN_STRING: {
                assert(!expecting_op);
                peek_token(cu, &t2);
                if (t2.type == TOKEN_PAREN_OPEN) {
                    void_lookahead_token(cu);
                    char* fn_name = t1.str;
                    ureg ast_size = get_ast_size(cu);
                    parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                    recalc_sho_its(cu, &sho_root, &sho_ri, &sho_re);
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str = fn_name;
                    e++;
                    e->expr.size = get_ast_growth(cu, ast_size);
                    e->expr.type= EXPR_NODE_FN_CALL;
                }
                else if(t2.type == TOKEN_BRACE_OPEN){
                    void_lookahead_token(cu);
                    char* el_name = t1.str;
                    //we cant' just use a pointer here because we might have a realloc
                    //this ammends the - sizeof(ast_node) because its used in a subtraction
                    //so it's gonna be canceled out
                    ureg el_end= get_ast_size(cu);
                    parse_arg_list(cu, TOKEN_BRACE_CLOSE);
                    peek_token(cu, &t2);
                    if(t2.type == TOKEN_PAREN_OPEN){
                        void_lookahead_token(cu);
                        ureg generic_args_rstart = get_ast_size(cu);
                        parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                        recalc_sho_its(cu, &sho_root, &sho_ri, &sho_re);
                        ast_rel_ptr arg_list_size = (ast_rel_ptr)
                                ((get_ast_size(cu) - generic_args_rstart) /
                                sizeof(ast_node));
                        e = claim_ast_space(cu, sizeof(ast_node) * 3);
                        e->expr.size = arg_list_size + 1;
                        e++;
                        e->str = el_name;
                        e++;
                        e->expr.size= get_ast_growth(cu, el_end);
                        e->expr.type= EXPR_NODE_GENERIC_FN_CALL;
                    }
                    else{
                       //scoped type stuff
                    }
                }
                else if(t2.type == TOKEN_BRACKET_OPEN){
                    ureg el_end= get_ast_size(cu);
                    parse_arg_list(cu, TOKEN_BRACKET_CLOSE);
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str= t1.str;
                    e++;
                    e->expr.size= get_ast_growth(cu, el_end);
                    e->expr.type= EXPR_NODE_ARRAY_ACCESS;
                }
                else {
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str= t1.str;
                    e++;
                    e->expr.size = 2;
                    e->expr.type= EXPR_NODE_VARIABLE;
                }
                //true for all: fn call, var, array access and generic fn call
                expecting_op = true;
            }break;
            case TOKEN_EOF:CIM_ERROR("We reached eof inside an expression");return-1;
            default:{
lbl_default:;
                if(t1.type == term1 || t1.type == term2){
                    for(;sho_ri != sho_re; sho_ri--){
                        flush_shy_op(cu, sho_ri);
                    }
                    if(!sub_expr){
                        ast_node* decl;
                        decl = (ast_node*)(cu->ast.start + expr_start);
                        decl->common.size = get_ast_growth(cu, expr_start);
                        decl->common.type = ASTNT_EXPRESSION;
                    }
                    return (t1.type == term1) ? 0 : 1;
                }
                CIM_ERROR("Unexpected token");return-1;
            }
        }
        consume_token(cu, &t1);
    }
}
static int parse_expr(cunit *cu, token_type term1, token_type term2, bool sub_expr){
    ureg expr_start = get_ast_size(cu);
    if(!sub_expr)claim_ast_space(cu, sizeof(ast_node));
    ureg shy_ops_start = cu->parsr.shy_ops.head - cu->parsr.shy_ops.start;
    return continue_parse_expr(cu, term1, term2, sub_expr, expr_start, shy_ops_start,shy_ops_start,
                        false);
}
static u8 count_ptrs(cunit *cu){
    token t1;
    u8 ptrs = 0;
    while(true){
        peek_token(cu, &t1);
        if(t1.type == TOKEN_STAR){
            void_lookahead_token(cu);
            ptrs++;
        }
        else{
            return ptrs;
        }
    }
}
//consider passing one token as param to significantly reduce peeking
static ast_node* parse_type(cunit* cu){
    ureg ast_pos = get_ast_size(cu);
    ast_node* t;
    ast_node* tn;
    token t1;
    token t2;
    consume_token(cu, &t1);
    peek_token(cu, &t2);
    bool scoped= false;
    CIM_ASSERT(t1.type == TOKEN_STRING);
    if(t2.type == TOKEN_COLON){
        scoped = true;
        do{
            tn = claim_ast_space(cu, sizeof(ast_node));
            tn->str = t1.str;
            void_lookahead_token(cu);
            consume_token(cu, &t1);
            peek_token(cu, &t2);
        }while(t2.type == TOKEN_COLON);
        //t2 is peeked during the loop
    }
    if(t2.type == TOKEN_BRACE_OPEN){
        ureg post_scope_ast_pos = get_ast_size(cu);
         // generic struct, potentially pointer
        void_lookahead_token(cu);
        peek_token(cu, &t2);
        if(t2.type != TOKEN_BRACE_CLOSE){
            do{
                parse_type(cu);
                consume_token(cu, &t2);
            }while(t2.type == TOKEN_COMMA);
            CIM_ASSERT(t2.type == TOKEN_BRACE_CLOSE);
        }
        else{
            void_lookahead_token(cu);
        }
        ast_node* scoped_str;
        //might not be used, but then it's used by tn
        scoped_str = claim_ast_space(cu, sizeof(ast_node) * (2 + scoped));
        tn = scoped_str + scoped;
        t = (void*)(tn + 1);
        ureg final_ast_pos = get_ast_size(cu);
        t->type.size = (ast_rel_ptr)((final_ast_pos - ast_pos) / sizeof(ast_node));
        t->type.type = (scoped) ? EXPR_NODE_TYPE_SCOPED_GENERIC_STRUCT :
                             EXPR_NODE_TYPE_GENERIC_STRUCT;
        t->type.ptrs = count_ptrs(cu);
        if(scoped){
            scoped_str->str = t1.str;
            // -2 if not nested because the t and tn nodes are after ast_pos and we only capture
            //scope size
            tn->type.size  = (ast_rel_ptr)
                    ((final_ast_pos - post_scope_ast_pos) / sizeof(ast_node) - 3);
        }
        else{
            tn->str = t1.str;
        }
        peek_token(cu, &t2); //t2 peek is restored
    }
    else{
        tn = claim_ast_space(cu, sizeof(ast_node) * 2);
        t = (void*)(tn + 1);
        t->type.size  = (ast_rel_ptr)
                ((get_ast_size(cu) - ast_pos) / sizeof(ast_node));
        tn->str = t1.str;
        // simple type
        t->type.type = scoped ? EXPR_NODE_TYPE_SCOPED : EXPR_NODE_TYPE_SIMPLE;
        t->type.ptrs = count_ptrs(cu);
        peek_token(cu, &t2);//t2 peek is restored
    }
    //loop because a function pointer might be the return type of a function pointer
    while(t2.type == TOKEN_PAREN_OPEN){
        peek_2nd_token(cu, &t1);
        if(t1.type != TOKEN_STAR)break;
        // function pointer
        ast_rel_ptr ret_type_size = (ast_rel_ptr)
                ((get_ast_size(cu) - ast_pos) / sizeof(ast_node));
        void_lookahead_token(cu);
        void_lookahead_token(cu);
        u8 ptrs = count_ptrs(cu) + (u8)1;
        consume_token(cu, &t1);
        CIM_ASSERT(t1.type == TOKEN_PAREN_CLOSE);
        consume_token(cu, &t1);
        CIM_ASSERT(t1.type == TOKEN_PAREN_OPEN);
        peek_token(cu, &t2);
        if(t2.type != TOKEN_PAREN_CLOSE){
            do{
                parse_type(cu);
                consume_token(cu, &t2);
            }while(t2.type == TOKEN_COMMA);
            CIM_ASSERT(t2.type == TOKEN_PAREN_CLOSE);
        }
        else{
             void_lookahead_token(cu); //get rid of the closing paren
        }
        tn = claim_ast_space(cu, sizeof(ast_node) * 2);
        t = tn+1;
        t->type.size = (ast_rel_ptr)
                    (get_ast_size(cu) - ast_pos) / sizeof(ast_node);
        t->type.type = EXPR_NODE_TYPE_FN_PTR;
        t->type.ptrs = ptrs;
        tn->type.size = t->type.size - ret_type_size - 1;
        //doesnt have a name, not part of the type
        peek_token(cu, &t2);
    }
    while(t2.type == TOKEN_BRACKET_OPEN){
        void_lookahead_token(cu);
        peek_token(cu, &t2);
        ureg expr_start = get_ast_size(cu);
        parse_expr(cu, TOKEN_BRACKET_CLOSE, TOKEN_BRACKET_CLOSE, true);
        ast_rel_ptr exp_size = get_ast_growth(cu, expr_start);;
        t = claim_ast_space(cu, sizeof(ast_node) * 2);
        t->type.size = exp_size + 1;
        t++;
        t->type.type = EXPR_NODE_TYPE_ARRAY;
        t->type.ptrs = count_ptrs(cu);
        t->type.size = get_ast_growth(cu, ast_pos);
        peek_token(cu, &t2);
    }
    return t;
}
static int parse_meta(cunit* cu, token* t1){

}
static int parse_struct(cunit* cu, int mods){

}
static int parse_block(cunit* cu){
    token t1;
    ureg ast_start = get_ast_size(cu);
    claim_ast_space(cu, sizeof(ast_node));
    consume_token(cu, &t1);
    CIM_ASSERT(t1.type == TOKEN_BRACE_OPEN);
    int r;
    do{
      r = parse_elem(cu);
    } while(r == 0);
    CIM_ASSERT(r == 2);
    ast_node* n = (ast_node*)(cu->ast.start + ast_start);
    n->size = get_ast_size(cu) - ast_start;
    return 0;
}
static int parse_typedef(cunit* cu, int mods){
    //it is safe to void 1 lookahead, as KEYWORD_TYPEDEF must aways be looked ahead
    void_lookahead_token(cu);
    ureg ast_pos = get_ast_size(cu);
    astn_typedef* td = claim_ast_space(cu, sizeof(astn_typedef));
    td->type = ASTNT_TYPEDEF;
    token t;
    consume_token(cu, &t);
    td->tgt_type.str = t.str;
    parse_type(cu);
    td = (void*)(cu->ast.start + ast_pos);
    td->size = (ast_rel_ptr)
            ((get_ast_size(cu) - ast_pos - sizeof(astn_typedef))
             / sizeof(ast_node) - 1);
    consume_token(cu, &t);
    CIM_ASSERT(t.type == TOKEN_SEMICOLON);
    return 0;
}
static int parse_type_as_expr_begin(cunit *cu, ureg ast_start, ast_node *type, bool sub_expr,
                                    token_type term1, token_type term2, bool after_fn_call){
    //TODO: add scoped
    ureg shy_ops_start = dbuffer_get_size(&cu->parsr.shy_ops);
    ureg shy_ops_pos;
    //type type is identical to sub_expr_type var / scoped var
    if(type->type.ptrs){
        ast_node* sop = dbuffer_claim_small_space(&cu->parsr.shy_ops, sizeof(ast_node) * type->type.ptrs);
        sop->expr.special.opcode= OP_MULTIPLY;
        sop->expr.type = EXPR_NODE_OP_LR;
        sop++;
        while((void*)sop!= cu->parsr.shy_ops.head){
            sop->expr.type = EXPR_NODE_OP_L;
            sop->expr.special.opcode= OP_DEREFERENCE;
            sop++;
        }
        shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
    }
    else{
        shy_ops_pos = shy_ops_start;
    }
    if(type->type.type == EXPR_NODE_TYPE_SIMPLE){
        type->expr.type = EXPR_NODE_VARIABLE;
    }
    else if(type->type.type == EXPR_NODE_TYPE_SCOPED){
        type->expr.type = EXPR_NODE_SCOPED_VARIABLE;
    }
    return continue_parse_expr(cu, term1, term2, sub_expr, ast_start,
                               shy_ops_start,shy_ops_pos,
                               (after_fn_call == true || type->type.ptrs == 0));
};
static void change_param_list_to_arg_list(ast_node* rit, ast_node* rend){
    while(rit!= rend){
        char* param = rit->str;
        ast_node* t = rit-1;
        switch(t->type.type){
            case EXPR_NODE_TYPE_SIMPLE:{
                rit->expr.type = EXPR_NODE_CANCER_PTRS;
                rit->expr.special.ptrs = t->type.ptrs;
                rit->expr.size = 3;
                t->str = param;
            }break;
            case EXPR_NODE_TYPE_SCOPED:{
                rit->expr.type = EXPR_NODE_SCOPED_CANCER_PTRS;
                rit->expr.special.ptrs = t->type.ptrs;
                rit->expr.size = t->type.size + 1;
                t->str = param;
            }break;
            default: CIM_ERROR("Unexpected parameter type in change_param_list_to_arg_list");
        }
        rit -= rit->expr.size;
    }
}
static void change_generic_param_list_to_arg_list(ast_node* rit, ast_node* rend){
    while(rit!= rend){
        char* param = rit->str;
        ast_node* t = rit-1;
        switch(t->type.type){
            case EXPR_NODE_TYPE_SIMPLE:{
                rit->expr.type = EXPR_NODE_CANCER_PTRS;
                rit->expr.special.ptrs = t->type.ptrs;
                rit->expr.size = 3;
                t->str = param;
                rit -= 3;
            }break;
            case EXPR_NODE_TYPE_SCOPED:{
                rit->expr.type = EXPR_NODE_SCOPED_CANCER_PTRS;
                rit->expr.special.ptrs = t->type.ptrs;
                rit->expr.size = t->type.size + 1;
                t->str = param;
                rit -= rit->expr.size;
            }break;
            default: CIM_ERROR("Unexpected parameter type in change_generic_param_list_to_arg_list");
        }

    }
}
//returns 0 if it's ambiguous, 1 if it created a params list and 2 for a argument list
typedef enum arg_or_params_list_e{
    AOPL_ARG_LIST,
    AOPL_PARAM_LIST,
    AOPL_AMBIGUOUS, //in this case it was parsed as a param list
}arg_or_params_list;
static ast_rel_ptr parse_generic_args_list(cunit* cu){
    ureg ast_start = get_ast_size(cu);
    token t1, t2;
    peek_token(cu, &t1);
    while(t1.type != TOKEN_BRACE_CLOSE){
        if(t1.type != TOKEN_STRING){
            if(parse_expr(cu, TOKEN_COMMA, TOKEN_BRACE_CLOSE, true) == 1){
                return get_ast_growth(cu, ast_start);
            }
        }
        else{
            ast_node* t = parse_type(cu);
            peek_token(cu, &t1);
            if(t1.type == TOKEN_BRACE_CLOSE)break;
            if(t1.type != TOKEN_COMMA) {
                if (parse_type_as_expr_begin(cu,
                                             get_ast_size(cu) -
                                             t->type.size * sizeof(ast_node),
                                             t, true, TOKEN_COMMA, TOKEN_BRACE_CLOSE, false) == 1)
                {
                    return get_ast_growth(cu, ast_start);
                }
            }
            else{
                void_lookahead_token(cu);
            }
        }
        peek_token(cu, &t1);
    }
    void_lookahead_token(cu);
    return get_ast_growth(cu, ast_start);
}
static arg_or_params_list parse_generic_arg_or_params_list(cunit* cu){
    bool preparsed_type = true;
    ureg ast_start = get_ast_size(cu);
    token t1, t2;
    peek_token(cu, &t1);
    bool change_params_required = false;
    ast_node* t;
    ast_node* n;
    while(t1.type != TOKEN_BRACE_CLOSE){
        if(t1.type != TOKEN_STRING){
            preparsed_type = false;
            goto its_an_arg_list;
        }
        t = parse_type(cu);
        peek_token(cu, &t1);
        if(t1.type != TOKEN_STRING)goto its_an_arg_list;
        if(t->type.ptrs == 0){
            n = claim_ast_space(cu, sizeof(ast_node));
            n->str = t1.str;
            void_lookahead_token(cu);
            parse_param_list(cu, TOKEN_BRACE_CLOSE);
            return AOPL_PARAM_LIST;
        }
        peek_2nd_token(cu, &t2);
        if(t2.type != TOKEN_BRACE_CLOSE){
             if(t2.type != TOKEN_COMMA) goto its_an_arg_list;
        }
        else{
            n = claim_ast_space(cu, sizeof(ast_node));
            n->str = t1.str;
            void_2_lookahead_tokens(cu);
            return AOPL_AMBIGUOUS;
        }
        n = claim_ast_space(cu, sizeof(ast_node));
        n->str = t1.str;
        void_2_lookahead_tokens(cu);
        peek_token(cu, &t1);
        change_params_required = true;
    }
    return AOPL_AMBIGUOUS;
its_an_arg_list:;
    if(!preparsed_type){
        if(change_params_required){
                change_generic_param_list_to_arg_list(
                        (ast_node*)(cu->ast.head) -1,
                        (ast_node*)(cu->ast.start + ast_start) -1);
        }
        parse_generic_args_list(cu);
        return AOPL_ARG_LIST;
    }
    else{
        if(change_params_required){
                change_generic_param_list_to_arg_list(
                        (ast_node*)(cu->ast.head) -1 - t->type.size,
                        (ast_node*)(cu->ast.start + ast_start) -1);
        }
        if(t1.type != TOKEN_COMMA && t1.type != TOKEN_BRACE_CLOSE){
            if(parse_type_as_expr_begin(cu, get_ast_size(cu) - t->type.size,
                                        t, true, TOKEN_COMMA, TOKEN_BRACE_CLOSE, false) == 1)
            {
                return AOPL_ARG_LIST;
            };
        }
        parse_generic_args_list(cu);
        return AOPL_ARG_LIST;
    }
}
static arg_or_params_list parse_arg_or_param_list(cunit* cu){
    bool preparsed_type = true;
    ureg ast_start = get_ast_size(cu);
    token t1;
    token t2;
    peek_token(cu, &t1);
    bool change_params_required = false;
    ast_node* t;
    while(t1.type != TOKEN_PAREN_CLOSE){
        if(t1.type != TOKEN_STRING){
            preparsed_type = false;
            goto its_an_arg_list;
        }
        t = parse_type(cu);
        peek_token(cu, &t1);
        if(t->type.type == EXPR_NODE_TYPE_SIMPLE || t->type.type == EXPR_NODE_TYPE_SCOPED) {
            if (t1.type == TOKEN_PAREN_CLOSE || t1.type == TOKEN_COMMA) goto its_an_arg_list;
            if(t->type.ptrs == 0){
                if(t1.type == TOKEN_STRING) goto its_a_param_list;
                goto its_an_arg_list;
            }
            else{
                peek_2nd_token(cu, &t2);
                if(t1.type != TOKEN_STRING) goto its_an_arg_list;
                char* param_name = t1.str;
                if(t2.type != TOKEN_PAREN_CLOSE){
                    if(t2.type != TOKEN_COMMA) goto its_an_arg_list;
                    void_2_lookahead_tokens(cu);
                    change_params_required = true;
                    ast_node* n = claim_ast_space(cu, sizeof(ast_node));
                    n->str = param_name;
                    peek_token(cu, &t1);
                }
                else{
                    void_2_lookahead_tokens(cu);
                    ast_node* n = claim_ast_space(cu, sizeof(ast_node));
                    n->str = param_name;
                    return AOPL_AMBIGUOUS;
                }
            }
        }
        else{
            goto its_a_param_list;
        }
    }
    void_lookahead_token(cu);
    return AOPL_AMBIGUOUS;
its_a_param_list:;
    //handling the current parameter
    ast_node* n = claim_ast_space(cu, sizeof(ast_node));
    consume_token(cu, &t1);
    CIM_ASSERT(t1.type == TOKEN_STRING);
    n->str = t1.str;
    parse_param_list(cu, TOKEN_PAREN_CLOSE);
    return AOPL_PARAM_LIST;
its_an_arg_list:;
    ureg ast_pos;
    ureg list_size;
    if(preparsed_type || change_params_required){
        ast_pos = get_ast_size(cu);
        list_size = (ast_pos - ast_start);
    }
    if(change_params_required) {
        ureg preparsed_type_size = 0;
        if (preparsed_type) {
            preparsed_type_size = t->type.size * sizeof(ast_node);
            list_size -= preparsed_type_size;
        }
        change_param_list_to_arg_list((ast_node *)(cu->ast.head - preparsed_type_size) - 1,
           (ast_node *)(cu->ast.head - preparsed_type_size - list_size) - 1);
    }
    if(preparsed_type){
        if(parse_type_as_expr_begin(cu, ast_start+list_size, t,true,
                                    TOKEN_COMMA, TOKEN_PAREN_CLOSE, false) == 1)
        {
            return AOPL_ARG_LIST;
        }
    }
    while (parse_expr(cu, TOKEN_COMMA, TOKEN_PAREN_CLOSE, true) == 0);
    return AOPL_ARG_LIST;
}
static int parse_generic_function_decl_after_type(cunit* cu, int mods, ureg ast_start){
    token t1, t2;
    consume_token(cu, &t1);
    consume_token(cu, &t2);
    CIM_ASSERT(t1.type == TOKEN_STRING);
    CIM_ASSERT(t2.type == TOKEN_BRACE_OPEN);
    ureg ast_pos_pre_generic_params = get_ast_size(cu);
    parse_param_list(cu, TOKEN_BRACE_CLOSE);
    add_size_node(cu, ast_pos_pre_generic_params);
    ureg ast_pos_pre_params = get_ast_size(cu);
    consume_token(cu, &t2);
    CIM_ASSERT(t2.type == TOKEN_PAREN_OPEN);
    parse_param_list(cu, TOKEN_PAREN_CLOSE);
    ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
    n->expr.size = get_ast_growth(cu, ast_pos_pre_params) - 1;
    n++;
    n->str = t1.str;
    n++;
    n = (ast_node*)(cu->ast.start + ast_start);
    n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
    n->common.size = get_ast_growth(cu, ast_start);
    return parse_block(cu);
}
static int parse_function_decl_after_type(cunit *cu, int mods, ureg ast_start){
    token t1, t2;
    consume_token(cu, &t1);
    consume_token(cu, &t2);
    CIM_ASSERT(t1.type == TOKEN_STRING);
    CIM_ASSERT(t2.type == TOKEN_PAREN_OPEN);
    ureg ast_pos_pre_params = get_ast_size(cu);
    peek_token(cu, &t2);
    parse_param_list(cu, TOKEN_PAREN_CLOSE);
    ast_node* s = claim_ast_space(cu, sizeof(ast_node) * 2);
    s->common.size = get_ast_growth(cu, ast_pos_pre_params) - 1;
    s++;
    s->str = t1.str;
    s = (ast_node*)(cu->ast.start + ast_start);
    s->common.type = ASTNT_FUNCTION_DECLARATION;
    s->common.size = get_ast_growth(cu, ast_start);
    return parse_block(cu);
}
static inline int parse_modifiers(cunit* cu, token* t){
    int mods = 0;
    while(true){
        if(t->str == KEYWORD_CONST)mods |= MOD_CONST;
        else if(t->str == KEYWORD_PUBLIC)mods|=MOD_PUBLIC;
        else break;
        //TODO: add missing mods
        consume_token(cu, t);
    }
    return mods;
}
static inline int parse_leading_string(cunit* cu){
    ureg ast_start = get_ast_size(cu);
    claim_ast_space(cu, sizeof(ast_node));
    ast_node* t = parse_type(cu);
    ast_rel_ptr type_size = t->type.size;
    token t1, t2;
    peek_token(cu, &t1);
    peek_2nd_token(cu, &t2);
    if(t1.type == TOKEN_STRING && t2.type == TOKEN_SEMICOLON){
        //var declaration
        //TODO: assigning declarations
        clear_lookahead(cu);
        ast_node* n = (ast_node*)(cu->ast.start + ast_start);
        n->var_decl.type = ASTNT_VARIABLE_DECLARATION;
        n->var_decl.size = (ast_rel_ptr)(t - n + 2);
        n->var_decl.assigning = false;
        n = claim_ast_space(cu, sizeof(ast_node));
        n->str = t1.str;
        return 0;
    }
    switch(t->type.type) {
        case EXPR_NODE_TYPE_SIMPLE: {
            if (t1.type == TOKEN_PAREN_OPEN && t->type.ptrs == 0) {
                //function call
                void_lookahead_token(cu);
                char *fn_name = (t - 1)->str;
                cu->ast.head = (void *) (t - 1); //we have to override these nodes :(
                ast_rel_ptr args_size = parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                ast_node *e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->str = fn_name;
                e++;
                e->expr.size = args_size + 2;
                e->expr.type = EXPR_NODE_FN_CALL;
                ureg shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start, shy_ops_pos,shy_ops_pos, true);
                return 0;
            }
            else if(t1.type == TOKEN_STRING){
                if(t2.type == TOKEN_PAREN_OPEN){
                    //+sizeof(ast_node) because of the leading expression node
                    if(t->type.ptrs == 0) return parse_function_decl_after_type(cu, 0, ast_start);
                    ureg arg_list_start = ast_start + sizeof(ast_node) +
                                          type_size * sizeof(ast_node);
                    char* fn_name = t1.str;
                    void_2_lookahead_tokens(cu);
                    ast_rel_ptr size_inc_as_arg_list;
                    arg_or_params_list r = parse_arg_or_param_list(cu);
                    if(r == AOPL_AMBIGUOUS){
                        peek_token(cu, &t1);
                        if(t1.type == TOKEN_BRACE_OPEN){
                            r = AOPL_PARAM_LIST;
                        }
                        else{
                            ureg arg_list_size = get_ast_growth(cu, arg_list_start);
                            change_param_list_to_arg_list((ast_node*)cu->ast.head-1,
                                 (ast_node*)cu->ast.head - arg_list_size - 1);
                            r = AOPL_ARG_LIST;
                        }
                    }
                    if(r == AOPL_PARAM_LIST){
                        add_size_node(cu, arg_list_start);
                        ast_node* n = claim_ast_space(cu, sizeof(ast_node));
                        n->str = fn_name;
                        n = (void*)(cu->ast.start + ast_start);
                        n->common.type = ASTNT_FUNCTION_DECLARATION;
                        n->common.size = get_ast_growth(cu, ast_start);
                        return parse_block(cu);
                    }
                    else {
                        ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
                        n->str = fn_name;
                        n++;
                        n->expr.type = EXPR_NODE_FN_CALL;
                        // we start at arg list start because the leading 'type'
                        // was actually a var * ... expression
                        n->expr.size = get_ast_growth(cu, arg_list_start);
                        //not -sizeof(ast_node) because of the expr node
                        t = (ast_node*)(cu->ast.start + ast_start) + type_size;
                        return parse_type_as_expr_begin(cu, ast_start, t, false,
                                                 TOKEN_SEMICOLON, TOKEN_SEMICOLON,true);
                    }
                }
                else if (t2.type == TOKEN_BRACE_OPEN){
                    if(t->type.ptrs == 0){
                        return parse_generic_function_decl_after_type(cu, 0, ast_start);
                    }
                    ureg generic_arg_list_start = get_ast_size(cu);
                    char* fn_name = t1.str;
                    void_2_lookahead_tokens(cu);
                    arg_or_params_list r = parse_generic_arg_or_params_list(cu);
                    ureg arg_list_start;
                    consume_required_token(cu, TOKEN_PAREN_OPEN);
                    if(r == AOPL_AMBIGUOUS){
                        add_size_node(cu, generic_arg_list_start); //we assume decl again
                        arg_list_start = get_ast_size(cu);
                        r= parse_arg_or_param_list(cu);
                        if(r == AOPL_AMBIGUOUS){
                            peek_token(cu, &t1);
                            if(t1.type == TOKEN_BRACE_OPEN){
                                r = AOPL_PARAM_LIST;
                            }
                            else{
                                r = AOPL_ARG_LIST;
                                ast_node* arg_list_pos = (void*)(cu->ast.start + arg_list_start);
                                //remove the size node for the generic arg list
                                memmove(arg_list_pos-1, arg_list_pos,
                                        cu->ast.head - (u8*)arg_list_pos);
                                cu->ast.head-=sizeof(ast_node);
                                change_param_list_to_arg_list((ast_node*)cu->ast.head - 1,
                                                              arg_list_pos-2);
                                arg_list_start-=sizeof(ast_node);
                                change_generic_param_list_to_arg_list(
                                        (ast_node*)(cu->ast.start + arg_list_start) - 1,
                                        (ast_node*)(cu->ast.start + generic_arg_list_start) - 1);

                            };
                        }
                        else if(r == AOPL_PARAM_LIST){
                            r = AOPL_PARAM_LIST;
                        }
                        else{
                            r = AOPL_ARG_LIST;
                            ast_node* arg_list_pos = (void*)(cu->ast.start + arg_list_start);
                            //remove the size node for the generic arg list
                            memmove(arg_list_pos-1, arg_list_pos,
                                    cu->ast.head - (u8*)arg_list_pos);
                            cu->ast.head-=sizeof(ast_node);
                            arg_list_start-=sizeof(ast_node);
                            change_generic_param_list_to_arg_list(
                                    (ast_node*)(cu->ast.start + arg_list_start) - 1,
                                    (ast_node*)(cu->ast.start + generic_arg_list_start) - 1);
                        }
                    }
                    else{
                        if(r == AOPL_PARAM_LIST){
                            add_size_node(cu, generic_arg_list_start);
                            arg_list_start = get_ast_size(cu);
                            parse_param_list(cu, TOKEN_PAREN_CLOSE);
                        }
                        else{
                            arg_list_start = get_ast_size(cu);
                            parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                        }
                    }
                    if(r == AOPL_PARAM_LIST){
                        ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
                        //exclude one node because of the fn name
                        n->expr.size = get_ast_growth(cu, arg_list_start + sizeof(ast_node));
                        n++;
                        n->str = fn_name;
                        n = (void*)(cu->ast.start + ast_start);
                        n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
                        n->common.size = get_ast_growth(cu, ast_start);
                        return parse_block(cu);
                    }
                    else {
                        ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 3);
                        //exclude the two following nodes
                        n->expr.size =
                                get_ast_growth(cu, arg_list_start + sizeof(ast_node) * 2);
                        n++;
                        n->str = fn_name;
                        n++;
                        n->expr.type = EXPR_NODE_GENERIC_FN_CALL;
                        //we start at arg list start because the leading 'type'
                        // was actually a var * ... expression
                        n->expr.size = get_ast_growth(cu, generic_arg_list_start);
                        //not -1 because of the expr node
                        t = (ast_node*)(cu->ast.start + ast_start) + type_size;
                        return parse_type_as_expr_begin(cu, ast_start, t, false,
                                                 TOKEN_SEMICOLON, TOKEN_SEMICOLON,true);
                    }
                }

            }

            //expression
           return parse_type_as_expr_begin(cu, ast_start, t,false, TOKEN_SEMICOLON,
                                           TOKEN_SEMICOLON, false);
        }
        case EXPR_NODE_TYPE_GENERIC_STRUCT: {
            if (t1.type == TOKEN_PAREN_OPEN) {
                //generic function call
                void_lookahead_token(cu);
                char *fn_name = (t - 1)->str;
                cu->ast.head = (void *) (t - 1); //we have to override these nodes :(
                ast_rel_ptr args_size = parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                ast_node *e = claim_ast_space(cu, sizeof(ast_node) * 3);
                e->expr.size = args_size + 1;
                e++;
                e->str = fn_name;
                e++;
                e->expr.size = get_ast_growth(cu, ast_start) - 1;//the one belongs to expr
                e->expr.type = EXPR_NODE_GENERIC_FN_CALL;
                ureg shy_ops_start = dbuffer_get_size(&cu->parsr.shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start,shy_ops_start , shy_ops_start, true);
                return 0;
            }
        }
        default:{
            if(t1.type == TOKEN_STRING && t2.type == TOKEN_PAREN_OPEN){
                return parse_function_decl_after_type(cu, 0, ast_start);

            }
            return -1;
        }
    }
}
static inline int parse_elem(cunit* cu){
    token t1;
    peek_token(cu, &t1);
    switch(t1.type) {
        case TOKEN_STRING: {
            //can only ever be a type decl with a leading string
            token t2;
            token t3;
            peek_2nd_token(cu, &t2);
            if (t2.type == TOKEN_STRING) {
                int mods = parse_modifiers(cu, &t1);
                //t2 is invalidated, t1 is pointing to first non modifier token
                if(t1.str == KEYWORD_STRUCT)return parse_struct(cu, mods);
                if(t1.str == KEYWORD_TYPEDEF)return parse_typedef(cu, mods);
                peek_3rd_token(cu, &t3);
                if(t3.type == TOKEN_PAREN_OPEN){
                    ureg ast_pos = get_ast_size(cu);
                    claim_ast_space(cu, sizeof(ast_node));
                    parse_type(cu);
                    return parse_function_decl_after_type(cu, mods, ast_pos);
                };
                if(t3.type == TOKEN_BRACE_OPEN){
                    ureg ast_pos = get_ast_size(cu);
                    claim_ast_space(cu, sizeof(ast_node));
                    parse_type(cu);
                    return parse_generic_function_decl_after_type(cu, mods, ast_pos);
                }
                return parse_leading_string(cu); //TODO: implement this branch
            }
            if(t1.str == KEYWORD_CONST && t2.type == TOKEN_PAREN_OPEN){
                ureg ast_pos = get_ast_size(cu);
                claim_ast_space(cu, sizeof(ast_node));
                parse_type(cu);
                peek_token(cu, &t1);
                if(t1.type == TOKEN_PAREN_OPEN) {
                    return parse_function_decl_after_type(cu, 0, ast_pos);
                }
                if(t1.type == TOKEN_BRACE_OPEN){
                    return parse_generic_function_decl_after_type(cu, 0, ast_pos);
                }
                CIM_ERROR("Unexpected token following type declaration");
            }
           return parse_leading_string(cu);
        }
        case TOKEN_HASH:
        case TOKEN_DOUBLE_HASH: {
            return parse_meta(cu, &t1);
        }
        case TOKEN_EOF: {
            clear_lookahead(cu);
            return 1;
        }
        case TOKEN_BRACE_CLOSE:{
            void_lookahead_token(cu);
            return 2;
        }
        default: {
            return parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false);
        }
    }
}
static void parse_file_scope(cunit* cu){
    while (parse_elem(cu) == 0);
}
void parse_file(cunit* cu, char* filename){
    tokenizer_open_file(cu, filename);
    parse_file_scope(cu);
    tokenizer_close_file(cu);
}
