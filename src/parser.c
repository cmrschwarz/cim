#include "parser.h"
#include <stdio.h>
#include "ast.h"
#include <assert.h>
#include "error.h"
#include "keywords.h"
#include "compiler.h"
#include <stdarg.h>

#define OP_RANGE (1 << (sizeof(u8)*8))

typedef enum arg_or_params_list_e{
    AOPL_ARG_LIST,
    AOPL_PARAM_LIST,
    AOPL_AMBIGUOUS, //in this case it was parsed as a param list
}arg_or_params_list;
enum assocs{
    LEFT_ASSOCIATIVE = 0,
    RIGHT_ASSOCIATIVE = 1,
};

static inline int parse_elem(cunit* cu, token_type end_tok);
static int parse_expr(cunit *cu, token_type term1, token_type term2, bool sub_expr);
static ast_node* parse_type(cunit* cu);
static arg_or_params_list parse_generic_arg_or_params_list(cunit* cu);

static u8 prec_table[OP_RANGE];
static u8 assoc_table[OP_RANGE];



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
    [OP_UNARY_PLUS] = 14,
    [OP_UNARY_MINUS] = 14,

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
    dbuffer_init_with_capacity(&cu->string_ptrs, 32);
    dbuffer_init(&cu->ast);
    dbuffer_init_with_capacity(&cu->parsr.shy_ops, 1000);
    dbuffer_init_with_capacity(&cu->tknzr.file_buffer, 8192);
    cu->tknzr.file = NULL;

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
    dbuffer_fin(&cu->tknzr.file_buffer);
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
static inline void add_size_node_abs(cunit* cu, ast_rel_ptr siz){
    ast_node* n = claim_ast_space(cu, sizeof(ast_node));
    n->expr.size = siz;
}
static void require_token(cunit *cu, token *t, token_type tt){
    if(t->type != tt){
        syntax_error(cu, t, 1, 0, "syntax error: unexpected token: wanted %s, got %s",
                     get_token_type_str(cu, tt), make_token_string(cu, t));
    }
}
static inline void consume_required_token(cunit* cu, token_type t){
    token* t1;
    t1 = consume_token(cu);
    require_token(cu, t1, t);
}
static inline void flush_shy_op(cunit* cu, ast_node* s){
    ast_node* expr_rit = (void*)(cu->ast.head - sizeof(ast_node));
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
    token* t;
    t = peek_token(cu);
    if(t->type == end_tok){
        void_lookahead_token(cu);
        return 0;
    }
    while (parse_expr(cu, TOKEN_COMMA, end_tok, true) == 0);
    return get_ast_growth(cu, ast_pos);
}
static inline void parse_param_list(cunit* cu, token_type end_tok){
    token* t1;
    t1 = peek_token(cu);
    if(t1->type != end_tok){
        do{
            ast_rel_ptr siz = parse_type(cu)->type.size + 2;
            ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
            t1 = consume_token(cu);
            require_token(cu, t1, TOKEN_STRING);
            n->str = t1->str;
            n++;
            n->type.type = EXPR_NODE_TYPE_PARAM;
            n->type.size = siz;
            t1 = consume_token(cu);
        }while(t1->type == TOKEN_COMMA);
        require_token(cu, t1, end_tok);
    }
    else{
        void_lookahead_token(cu);
    }
}
static int error_expecting_op(cunit* cu, token* t1, token_type term1, token_type term2){
     if(term1 == term2){
         syntax_error(cu, t1, 1, 1,
                      "expression syntax error: wanted an operation or %s, got %s",
                      get_token_type_str(cu, term1), get_token_type_str(cu, t1->type));
    }
    else{
         syntax_error(cu, t1, 1, 1,
                      "expression syntax error: wanted an operation "
                              "or %s or %s, got %s",
                      get_token_type_str(cu, term1),
                      get_token_type_str(cu, term2),
                      get_token_type_str(cu, t1->type));
    }
}
static int continue_parse_expr(cunit* cu, token_type term1, token_type term2, bool sub_expr,
                               ureg expr_start, ureg shy_ops_start,ureg shy_op_pos,
                               bool expecting_op)
{
    int x = 1;
    int y = x ++ + ++x;
    token* t1;
    token* t2;
    ast_node* e;
    ast_node sop;
    u8 prec;
    ast_node* sho_root = (void*)(cu->parsr.shy_ops.start);
    ast_node* sho_re = (ast_node*)(cu->parsr.shy_ops.start + shy_ops_start) - 1;
    ast_node* sho_ri = (ast_node*)(cu->parsr.shy_ops.start + shy_op_pos) - 1;
    ureg open_paren_count = 0;
    t1 = consume_token(cu);
    while(true){
        switch(t1->type){
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
                sop.expr.special.opcode= (u8)(t1->type);
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
            case TOKEN_BRACKET_OPEN:{
                if(!expecting_op) syntax_error(cu, t1, 1, 1, "this operation is not allowed here");
                t2 = peek_token(cu);
                if(t2->type == TOKEN_BRACKET_CLOSE) {
                    syntax_error(cu, t2, 2, 2, "array access missing index expression");
                }
                ureg ast_size = get_ast_size(cu);
                parse_expr(cu, TOKEN_BRACKET_CLOSE, TOKEN_BRACKET_CLOSE, true);
                ast_node* n = claim_ast_space(cu, sizeof(ast_node));
                n->expr.type = EXPR_NODE_ARRAY_ACCESS;
                n->full_size = get_ast_growth(cu, ast_size);
                expecting_op = true;
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
                if(expecting_op) error_expecting_op(cu, t1, term1, term2);
                e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->str = t1->str;
                e++;
                e->expr.size = 2;
                e->expr.type= (expr_node_type)t1->type;
                expecting_op = true;
            }break;
            case TOKEN_STRING: {
                if(expecting_op) error_expecting_op(cu, t1, term1, term2);
                t2 = peek_token(cu);
                if (t2->type == TOKEN_PAREN_OPEN) {
                    void_lookahead_token(cu);
                    ureg ast_size = get_ast_size(cu);
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str = t1->str;
                    e++;
                    e->type.type = EXPR_NODE_TYPE_SIMPLE;
                    e->type.size = 2;
                    ast_rel_ptr args_siz = parse_arg_list(cu, TOKEN_PAREN_CLOSE) + 1;
                    recalc_sho_its(cu, &sho_root, &sho_ri, &sho_re);
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->expr.size = args_siz;
                    e++;
                    e->expr.size = get_ast_growth(cu, ast_size);
                    e->expr.type= EXPR_NODE_FN_CALL;
                }
                else if(t2->type == TOKEN_BRACE_OPEN){
                    void_lookahead_token(cu);
                    ureg el_end= get_ast_size(cu);
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str = t1->str;
                    e++;
                    e->type.type = EXPR_NODE_TYPE_SIMPLE;
                    e->type.size = 2;
                    //we cant' just use a pointer here because we might have a realloc
                    //this ammends the - sizeof(ast_node) because its used in a subtraction
                    //so it's gonna be canceled out
                    parse_arg_list(cu, TOKEN_BRACE_CLOSE);
                    t2 = peek_token(cu);
                    if(t2->type == TOKEN_PAREN_OPEN){
                        void_lookahead_token(cu);
                        ureg generic_args_rstart = get_ast_size(cu);
                        parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                        recalc_sho_its(cu, &sho_root, &sho_ri, &sho_re);
                        ast_rel_ptr arg_list_size = (ast_rel_ptr)
                                ((get_ast_size(cu) - generic_args_rstart) /
                                sizeof(ast_node));
                        e = claim_ast_space(cu, sizeof(ast_node) * 2);
                        e->expr.size = arg_list_size + 1;
                        e++;
                        e->expr.size= get_ast_growth(cu, el_end);
                        e->expr.type= EXPR_NODE_GENERIC_FN_CALL;
                    }
                    else{
                       //scoped type stuff
                    }
                }
                else {
                    e = claim_ast_space(cu, sizeof(ast_node) * 2);
                    e->str= t1->str;
                    e++;
                    e->expr.size = 2;
                    e->expr.type= EXPR_NODE_VARIABLE;
                }
                //true for all: fn call, var, array access and generic fn call
                expecting_op = true;
            }break;
            case TOKEN_EOF:{
                syntax_error(cu, t1, 1, 1,
                             "expression syntax error: reached end of file before the end of the expression");
            }return-1;
            default:{
lbl_default:;
                if(!expecting_op){
                    syntax_error(cu, t1, 1, 1,
                                 "expression syntax error: reached %s before reaching a valid "
                                         "end to the expression",
                                 get_token_type_str(cu, t1->type));
                }
                if(t1->type == term1 || t1->type == term2){
                    for(;sho_ri != sho_re; sho_ri--){
                        flush_shy_op(cu, sho_ri);
                    }
                    if(!sub_expr){
                        ast_node* decl;
                        decl = (ast_node*)(cu->ast.start + expr_start);
                        decl->common.size = get_ast_growth(cu, expr_start);
                        decl->common.type = ASTNT_EXPRESSION;
                    }
                    return (t1->type == term1) ? 0 : 1;
                }
                else{
                    if(term1 == term2){
                        syntax_error(cu, t1, 1, 0,
                                     "expression syntax error: wanted %s, got %s",
                                     get_token_type_str(cu, term1),
                                     get_token_type_str(cu, t1->type));
                    }
                    else{
                        syntax_error(cu, t1, 1, 0,
                                     "expression syntax error: wanted %s or %s, got %s",
                                     get_token_type_str(cu, term1),
                                     get_token_type_str(cu, term2),
                                     get_token_type_str(cu, t1->type));
                    }
                }

            }
        }
        t1 = consume_token(cu);
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
    token* t1;
    u8 ptrs = 0;
    while(true){
        t1 = peek_token(cu);
        if(t1->type == TOKEN_STAR){
            void_lookahead_token(cu);
            ptrs++;
        }
        else{
            return ptrs;
        }
    }
}
static ast_node* parse_type(cunit* cu){
    bool scoped = false;
    ureg ast_pos = get_ast_size(cu);
    ast_node* t;
    ast_node* tn;
    token* t1;
    token* t2;
    while(true){
        t1 = consume_token(cu);
        require_token(cu, t1, TOKEN_STRING);
        tn = claim_ast_space(cu, sizeof(ast_node) * 2);
        t = (void*)(tn + 1);
        tn->str = t1->str;
        t->type.ptrs = count_ptrs(cu);
        if(scoped){
            t->type.size = get_ast_growth(cu, ast_pos);
            t->type.type = EXPR_NODE_TYPE_SCOPED;
        }
        else{
            t->type.size = 2;
            t->type.type = EXPR_NODE_TYPE_SIMPLE;
        }
        t2 = peek_token(cu);
        if(t2->type == TOKEN_BRACE_OPEN){
            void_lookahead_token(cu);
            t2 = peek_token(cu);
            ureg ast_pos_pre_args = get_ast_size(cu);
            arg_or_params_list r = AOPL_PARAM_LIST;
            if(t2->type != TOKEN_BRACE_CLOSE){
                r = parse_generic_arg_or_params_list(cu);
            }
            else{
                void_lookahead_token(cu);
            }
            t = claim_ast_space(cu, sizeof(ast_node) * 2);
            t->type.size = get_ast_growth(cu, ast_pos_pre_args) - 1;
            t++;
            t->type.size = get_ast_growth(cu, ast_pos);
            switch (r){
                case AOPL_AMBIGUOUS:
                    t->type.type =EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS;break;
                case AOPL_PARAM_LIST:
                    t->type.type = EXPR_NODE_TYPE_GENERIC_STRUCT_DEF;break;
                case AOPL_ARG_LIST:
                    t->type.type = EXPR_NODE_TYPE_GENERIC_STRUCT_INST; break;
            }
            t->type.ptrs = count_ptrs(cu);
            t2 = peek_token(cu);
        }
        if(t2->type != TOKEN_COLON) break;
        void_lookahead_token(cu);
        scoped = true;
    }
    //loop because a function pointer might be the return type of a function pointer
    while(t2->type == TOKEN_PAREN_OPEN){
        t1 = peek_nth_token(cu, 2);
        if(t1->type != TOKEN_STAR)break;
        // function pointer
        ast_rel_ptr ret_type_size = (ast_rel_ptr)
                ((get_ast_size(cu) - ast_pos) / sizeof(ast_node));
        void_lookahead_token(cu);
        void_lookahead_token(cu);
        u8 ptrs = count_ptrs(cu) + (u8)1;
        t1 = consume_token(cu);
        require_token(cu, t1, TOKEN_PAREN_CLOSE);
        t1 = consume_token(cu);
        require_token(cu, t1, TOKEN_PAREN_OPEN);
        t2 = peek_token(cu);
        if(t2->type != TOKEN_PAREN_CLOSE){
            do{
                parse_type(cu);
                t2 = consume_token(cu);
            }while(t2->type == TOKEN_COMMA);
            require_token(cu, t2, TOKEN_PAREN_CLOSE);
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
        t2 = peek_token(cu);
    }
    while(t2->type == TOKEN_BRACKET_OPEN){
        void_lookahead_token(cu);
        t2 = peek_token(cu);
        if(t2->type != TOKEN_BRACKET_CLOSE){
            parse_expr(cu, TOKEN_BRACKET_CLOSE, TOKEN_BRACKET_CLOSE, true);
            t = claim_ast_space(cu, sizeof(ast_node));
        }
        else{
            void_lookahead_token(cu);
            t = claim_ast_space(cu, sizeof(ast_node) * 2);
            t->expr.size = 1;
            t->expr.type = EXPR_NODE_NOP;
            t++;
        }
        t->type.type = EXPR_NODE_TYPE_ARRAY;
        t->type.ptrs = count_ptrs(cu);
        t->type.size = get_ast_growth(cu, ast_pos);
        t2 = peek_token(cu);
    }
    return t;
}
static int parse_meta(cunit* cu, token* t1){

}
static int parse_struct(cunit* cu, int mods){

}
static int parse_block(cunit* cu){
    token* t1;
    ureg ast_start = get_ast_size(cu);
    claim_ast_space(cu, sizeof(ast_node));
    t1 = consume_token(cu);
    require_token(cu, t1, TOKEN_BRACE_OPEN);
    int r;
    do{
      r = parse_elem(cu, TOKEN_BRACE_CLOSE);
    } while(r == 0);
    CIM_ASSERT(r == 1);
    ast_node* n = (ast_node*)(cu->ast.start + ast_start);
    n->full_size = get_ast_size(cu) - ast_start;
    return 0;
}
static int parse_typedef(cunit* cu, int mods){
    //it is safe to void 1 lookahead, as KEYWORD_TYPEDEF must aways be looked ahead
    void_lookahead_token(cu);
    ureg ast_pos = get_ast_size(cu);
    astn_typedef* td = claim_ast_space(cu, sizeof(astn_typedef));
    td->type = ASTNT_TYPEDEF;
    token* t;
    t = consume_token(cu);
    td->tgt_type.str = t->str;
    parse_type(cu);
    td = (void*)(cu->ast.start + ast_pos);
    td->size = (ast_rel_ptr)
            ((get_ast_size(cu) - ast_pos - sizeof(astn_typedef))
             / sizeof(ast_node) - 1);
    t = consume_token(cu);
    require_token(cu, t, TOKEN_SEMICOLON);
    return 0;
}
static void change_type_to_expr(cunit* cu, ast_node* t){
    switch (t->type.type){
        case EXPR_NODE_TYPE_SIMPLE: {
            t->type.ptrs = 0;
        }break;
        case EXPR_NODE_TYPE_GENERIC_STRUCT_DEF:
        case EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS:
        case EXPR_NODE_TYPE_GENERIC_STRUCT_INST:{
            t->type.ptrs = 0;
            change_type_to_expr(cu, t - (t-1)->type.size - 1);
        }break;
        case EXPR_NODE_TYPE_ARRAY:{
            t->expr.type = EXPR_NODE_ARRAY_ACCESS;
            t-=1;
            change_type_to_expr(cu, t-t->expr.size);
        }break;
        case EXPR_NODE_TYPE_SCOPED:{
            change_type_to_expr(cu, t-2);
        }break;
        default:{
            syntax_error(cu, peek_token(cu), 2, 0,
                         "failed to parse this expression");
        }break;
    }
}
static inline ureg add_sops_from_ptrs(cunit* cu, u8 ptrs){
    ast_node* sop = dbuffer_claim_small_space(&cu->parsr.shy_ops, sizeof(ast_node) * ptrs);
    sop->expr.special.opcode= OP_MULTIPLY;
    sop->expr.type = EXPR_NODE_OP_LR;
    sop++;
    while((void*)sop!= cu->parsr.shy_ops.head){
        sop->expr.type = EXPR_NODE_OP_L;
        sop->expr.special.opcode= OP_DEREFERENCE;
        sop++;
    }
}
static int parse_type_as_expr_begin(cunit *cu, ureg ast_start, ast_node *type, bool sub_expr,
                                    token_type term1, token_type term2, bool after_second)
{
    u8 ptrs = type->type.ptrs;
    ureg shy_ops_start = dbuffer_get_size(&cu->parsr.shy_ops);
    ureg shy_ops_pos;
    //type type is identical to sub_expr_type var / scoped var
    if(ptrs){
        add_sops_from_ptrs(cu, ptrs);
        shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
    }
    else{
        shy_ops_pos = shy_ops_start;
    }
    change_type_to_expr(cu, type);
    return continue_parse_expr(cu, term1, term2, sub_expr, ast_start,
                               shy_ops_start,shy_ops_pos,
                               (after_second == true || ptrs == 0));
};
static void change_param_list_to_arg_list(cunit* cu, ast_node* rit, ast_node* rend){
    while(rit!= rend){
        ast_node* t = rit-2;
        rit->expr.type = EXPR_NODE_CANCER_PTRS;
        rit->expr.special.ptrs = t->type.ptrs;
        rit->expr.size = 2 + t->type.size;
        rit-= rit->expr.size;
        change_type_to_expr(cu, t);
    }
}
static void change_generic_param_list_to_arg_list(cunit* cu, ast_node* rit, ast_node* rend){
    while(rit!= rend){
        ast_node* t = rit-2;
        rit->expr.type = EXPR_NODE_CANCER_PTRS;
        rit->expr.special.ptrs = t->type.ptrs;
        rit->expr.size = 2 + t->type.size;
        rit-= rit->expr.size;
    }
}

static ast_rel_ptr parse_generic_args_list(cunit* cu){
    ureg ast_start = get_ast_size(cu);
    token* t1;
    token* t2;
    t1 = peek_token(cu);
    while(t1->type != TOKEN_BRACE_CLOSE){
        if(t1->type != TOKEN_STRING){
            if(parse_expr(cu, TOKEN_COMMA, TOKEN_BRACE_CLOSE, true) == 1){
                return get_ast_growth(cu, ast_start);
            }
        }
        else{
            ast_node* t = parse_type(cu);
            t1 = peek_token(cu);
            if(t1->type == TOKEN_BRACE_CLOSE)break;
            if(t1->type != TOKEN_COMMA) {
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
        t1 = peek_token(cu);
    }
    void_lookahead_token(cu);
    return get_ast_growth(cu, ast_start);
}
static arg_or_params_list parse_generic_arg_or_params_list(cunit* cu){
    bool preparsed_type = true;
    ureg ast_start = get_ast_size(cu);
    token* t1;
    token* t2;
    t1 = peek_token(cu);
    bool change_params_required = false;
    ast_node* t;
    ast_node* n;
    while(t1->type != TOKEN_BRACE_CLOSE){
        if(t1->type != TOKEN_STRING){
            preparsed_type = false;
            goto its_an_arg_list;
        }
        t = parse_type(cu);
        t1 = peek_token(cu);
        if(t1->type != TOKEN_STRING)goto its_an_arg_list;
        if(t->type.ptrs == 0){
            ast_rel_ptr siz = t->type.size + 2;
            n = claim_ast_space(cu, sizeof(ast_node) * 2);
            n->str = t1->str;
            n++;
            n->type.type = EXPR_NODE_TYPE_PARAM;
            n->type.size = siz;
            void_lookahead_token(cu);
            t1 = consume_token(cu);
            if(t1->type == TOKEN_COMMA){
                parse_param_list(cu, TOKEN_BRACE_CLOSE);
            }
            else{
                require_token(cu, t1, TOKEN_BRACE_CLOSE);
            }
            return AOPL_PARAM_LIST;
        }
        t2 = peek_2nd_token(cu);
        if(t2->type != TOKEN_BRACE_CLOSE){
             if(t2->type != TOKEN_COMMA) goto its_an_arg_list;
        }
        else{
            ast_rel_ptr siz = t->type.size + 2;
            n = claim_ast_space(cu, sizeof(ast_node) * 2);
            n->str = t1->str;
            n++;
            n->type.type = EXPR_NODE_TYPE_PARAM;
            n->type.size = siz;
            void_lookahead_tokens(cu, 2);
            return AOPL_AMBIGUOUS;
        }
        ast_rel_ptr siz = t->type.size + 2;
        n = claim_ast_space(cu, sizeof(ast_node) * 2);
        n->str = t1->str;
        n++;
        n->type.type = EXPR_NODE_TYPE_PARAM;
        n->type.size = siz;
        void_lookahead_tokens(cu, 2);
        t1 = peek_token(cu);
        change_params_required = true;
    }
    return AOPL_AMBIGUOUS;
its_an_arg_list:;
    if(!preparsed_type){
        if(change_params_required){
                change_generic_param_list_to_arg_list(cu,
                        (ast_node*)(cu->ast.head) -1,
                        (ast_node*)(cu->ast.start + ast_start) -1);
        }
        parse_generic_args_list(cu);
        return AOPL_ARG_LIST;
    }
    else{
        if(change_params_required){
                change_generic_param_list_to_arg_list(cu,
                        (ast_node*)(cu->ast.head) -1 - t->type.size,
                        (ast_node*)(cu->ast.start + ast_start) -1);
        }
        if(t1->type != TOKEN_COMMA && t1->type != TOKEN_BRACE_CLOSE){
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
    token* t1;
    token* t2;
    t1 = peek_token(cu);
    bool change_params_required = false;
    ast_node* t;
    while(t1->type != TOKEN_PAREN_CLOSE){
        if(t1->type != TOKEN_STRING){
            preparsed_type = false;
            goto its_an_arg_list;
        }
        t = parse_type(cu);
        t1 = peek_token(cu);
        if(t->type.type == EXPR_NODE_TYPE_SIMPLE || t->type.type == EXPR_NODE_TYPE_SCOPED) {
            if (t1->type == TOKEN_PAREN_CLOSE || t1->type == TOKEN_COMMA) goto its_an_arg_list;
            if(t->type.ptrs == 0){
                if(t1->type == TOKEN_STRING) goto its_a_param_list;
                goto its_an_arg_list;
            }
            else{
                t2 = peek_2nd_token(cu);
                if(t1->type != TOKEN_STRING) goto its_an_arg_list;
                char* param_name = t1->str;
                if(t2->type != TOKEN_PAREN_CLOSE){
                    if(t2->type != TOKEN_COMMA) goto its_an_arg_list;
                    void_lookahead_tokens(cu, 2);
                    change_params_required = true;
                    ast_rel_ptr siz = t->type.size + 2;
                    ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
                    n->str = param_name;
                    n++;
                    n->type.type = EXPR_NODE_TYPE_PARAM;
                    n->type.size = siz;
                    t1 = peek_token(cu);
                }
                else{
                    void_lookahead_tokens(cu, 2);
                    ast_rel_ptr siz = t->type.size + 2;
                    ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
                    n->str = param_name;
                    n++;
                    n->type.type = EXPR_NODE_TYPE_PARAM;
                    n->type.size = siz;
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
    ast_rel_ptr siz = t->type.size + 2;
    ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
    n->str = t1->str;
    n++;
    n->type.type = EXPR_NODE_TYPE_PARAM;
    n->type.size = siz;
    t1 = consume_token(cu);
    CIM_ASSERT(t1->type == TOKEN_STRING);
    n->str = t1->str;
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
        change_param_list_to_arg_list(cu, (ast_node *)(cu->ast.head - preparsed_type_size) - 1,
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
    token* t1;
    t1 = consume_token(cu);
    require_token(cu, t1, TOKEN_STRING);
    ast_node* n = claim_ast_space(cu, 2 * sizeof(ast_node));
    n->str = t1->str;
    n++;
    n->type.size = 2;
    n->type.type = EXPR_NODE_TYPE_SIMPLE;
    t1 = consume_token(cu);
    require_token(cu, t1, TOKEN_BRACE_OPEN);
    ureg ast_pos_pre_generic_params = get_ast_size(cu);
    parse_param_list(cu, TOKEN_BRACE_CLOSE);
    add_size_node(cu, ast_pos_pre_generic_params);
    ureg ast_pos_pre_params = get_ast_size(cu);
    t1= consume_token(cu);
    require_token(cu, t1, TOKEN_PAREN_OPEN);
    parse_param_list(cu, TOKEN_PAREN_CLOSE);
    n = claim_ast_space(cu, sizeof(ast_node));
    n->expr.size = get_ast_growth(cu, ast_pos_pre_params);
    n = (ast_node*)(cu->ast.start + ast_start);
    n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
    n->common.size = get_ast_growth(cu, ast_start);
    return parse_block(cu);
}
static int parse_function_decl_after_type(cunit *cu, int mods, ureg ast_start){
    token* t1;
    t1 = consume_token(cu);
    require_token(cu, t1, TOKEN_STRING);
    ast_node* n = claim_ast_space(cu, 2 * sizeof(ast_node));
    n->str = t1->str;
    n++;
    n->type.size = 2;
    n->type.type = EXPR_NODE_TYPE_SIMPLE;
    t1 = consume_token(cu);
    require_token(cu, t1, TOKEN_PAREN_OPEN);
    ureg ast_pos_pre_params = get_ast_size(cu);
    parse_param_list(cu, TOKEN_PAREN_CLOSE);
    ast_node* s = claim_ast_space(cu, sizeof(ast_node));
    s->common.size = get_ast_growth(cu, ast_pos_pre_params);
    s = (ast_node*)(cu->ast.start + ast_start);
    s->common.type = ASTNT_FUNCTION_DECLARATION;
    s->common.size = get_ast_growth(cu, ast_start);
    return parse_block(cu);
}
static inline int parse_modifiers(cunit* cu, token* t){
    int mods = 0;
    while(true){
        if(str_eq_keyword(t->str, KEYWORD_CONST))mods |= MOD_CONST;
        else if(str_eq_keyword(t->str, KEYWORD_CONST))mods|=MOD_PUBLIC;
        else break;
        //TODO: add missing mods
        t = consume_token(cu);
    }
    return mods;
}
static inline int parse_leading_string(cunit* cu){
    ureg ast_start = get_ast_size(cu);
    claim_ast_space(cu, sizeof(ast_node));
    ast_node* t = parse_type(cu);
    u8 t_ptrs = t->type.ptrs;
    ast_rel_ptr type_size = t->type.size;
    token* t1;
    token* t2;
    t1 = peek_token(cu);
    if(t_ptrs == 0){
        //unambiguously parsable as a declaration
        if(t1->type == TOKEN_STRING){
            ast_node* rt2 = parse_type(cu);
            if(rt2->type.type == EXPR_NODE_TYPE_SIMPLE ||
               rt2->type.type == EXPR_NODE_TYPE_SCOPED)
            {
                t1 = consume_token(cu);
                if (t1->type == TOKEN_SEMICOLON) {
                    ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                    n->var_decl.type = ASTNT_VARIABLE_DECLARATION;
                    n->var_decl.size = get_ast_growth(cu, ast_start);
                    n->var_decl.assigning = false;
                    return 0;
                } else if (t1->type == TOKEN_EQUALS) {
                    ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                    parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, true);
                    n->var_decl.type = ASTNT_VARIABLE_DECLARATION;
                    n->var_decl.size = get_ast_growth(cu, ast_start);
                    n->var_decl.assigning = true;
                    return 0;
                }
                else if (t1->type == TOKEN_PAREN_OPEN){
                    ureg ast_pos_pre_params = get_ast_size(cu);
                    parse_param_list(cu, TOKEN_PAREN_CLOSE);
                    add_size_node(cu,  ast_pos_pre_params);
                    ast_node* s = (void*)(cu->ast.start + ast_start);
                    s->common.type = ASTNT_FUNCTION_DECLARATION;
                    s->common.size = get_ast_growth(cu, ast_start);
                    return parse_block(cu);
                }
                else{
                    syntax_error(cu, t1, 2, 0,
                                 "syntax error: got %s, wanted '=' or ';' or '(' to parse as variable or function declaration",
                                 make_token_string(cu, t1));
                }
            }
            else if(rt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_DEF
                    || rt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS)
            {
                // the rt2 type doesnn't matter, this node is used
                // for the size of the generic arg list
                t1 = consume_token(cu);
                require_token(cu, t1, TOKEN_PAREN_OPEN);
                add_size_node_abs(cu, parse_arg_list(cu, TOKEN_PAREN_CLOSE));
                parse_block(cu);
                 ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
                return 0;
            }
        }
        else if(t1->type == TOKEN_PAREN_OPEN){
            void_lookahead_token(cu);
            if(t->type.type == EXPR_NODE_TYPE_SIMPLE ||
               t->type.type == EXPR_NODE_TYPE_SCOPED)
            {
                ast_rel_ptr args_size = parse_arg_list(cu, TOKEN_PAREN_CLOSE) + 1;
                ast_node *e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->expr.size = args_size;
                e++;
                e->expr.size = get_ast_growth(cu, ast_start) - 1;
                e->expr.type = EXPR_NODE_FN_CALL;
                ureg shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start, shy_ops_pos,shy_ops_pos, true);
                return 0;
            }
            else if(t->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_INST ||
                    t->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS)
            {
                cu->ast.head -= sizeof(ast_node);
                if(t->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS){
                    change_param_list_to_arg_list(cu, (ast_node*)cu->ast.head - 2,
                                                  t - (t-1)->type.size -1);
                }
                ureg ast_pos_pre_args = get_ast_size(cu);
                parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                ast_node *e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->expr.size = get_ast_growth(cu, ast_pos_pre_args) - 1;
                e++;
                e->expr.size = get_ast_growth(cu, ast_start) -1;
                e->expr.type = EXPR_NODE_GENERIC_FN_CALL;
                ureg shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start, shy_ops_pos,shy_ops_pos, true);
            }
            else{
                syntax_error(cu, t1, 1, 0, "syntax error: invalid function call syntax");
            }
        }
        else{
            return parse_type_as_expr_begin(cu, ast_start, t, false,
                                            TOKEN_SEMICOLON, TOKEN_SEMICOLON, false);
        }
    }
    else{
        if(t1->type == TOKEN_STRING) {
            ast_node *tt2 = parse_type(cu);
            if (tt2->type.type == EXPR_NODE_TYPE_SIMPLE ||
                tt2->type.type == EXPR_NODE_TYPE_SCOPED)
            {
                t1 = peek_token(cu);
                if (t1->type == TOKEN_SEMICOLON) {
                    void_lookahead_token(cu);
                    ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                    n->var_decl.type = ASTNT_VARIABLE_DECLARATION_AMBIGUOUS;
                    n->var_decl.size = get_ast_growth(cu, ast_start);
                    n->var_decl.assigning = false;
                    return 0;
                }
                else if (t1->type == TOKEN_EQUALS) {
                    void_lookahead_token(cu);
                    ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                    parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, true);
                    n->var_decl.type = ASTNT_VARIABLE_DECLARATION_AMBIGUOUS;
                    n->var_decl.size = get_ast_growth(cu, ast_start);
                    n->var_decl.assigning = true;
                    return 0;
                }
                else if (t1->type == TOKEN_PAREN_OPEN) {
                    void_lookahead_token(cu);
                    ureg ast_pos_pre_params = get_ast_size(cu);
                    arg_or_params_list r = parse_arg_or_param_list(cu);
                    t1 = peek_token(cu);
                    if(t1->type == TOKEN_BRACE_OPEN){
                        add_size_node(cu, ast_pos_pre_params);
                        ast_node *s = (void *) (cu->ast.start + ast_start);
                        s->common.type = ASTNT_FUNCTION_DECLARATION;
                        s->common.size = get_ast_growth(cu, ast_start);
                        return parse_block(cu);
                    }
                    else{
                        if(r == AOPL_AMBIGUOUS){
                            change_param_list_to_arg_list(cu, (ast_node*)cu->ast.head,
                               (void*)(cu->ast.start + ast_pos_pre_params));
                        }
                        ast_node* s = claim_ast_space(cu, sizeof(ast_node) * 2);
                        s->expr.size = get_ast_growth(cu, ast_pos_pre_params) - 1;
                        s++;
                        s->expr.size = get_ast_growth(cu, ast_start) - type_size - 1;
                        s->expr.type = EXPR_NODE_FN_CALL;
                        t = (ast_node*)cu->ast.start + type_size;
                        return parse_type_as_expr_begin(cu, ast_start, t, false,
                                TOKEN_SEMICOLON, TOKEN_SEMICOLON, true);
                    }
                }
                else {
                    //expression
                    ureg shy_ops_start = dbuffer_get_size(&cu->parsr.shy_ops);
                    ureg shy_ops_pos;
                    add_sops_from_ptrs(cu, t_ptrs);
                    if (tt2->type.ptrs) add_sops_from_ptrs(cu, tt2->type.ptrs);
                    shy_ops_pos = dbuffer_get_size(&cu->parsr.shy_ops);
                    change_type_to_expr(cu, tt2);
                    change_type_to_expr(cu, tt2 - tt2->type.size);
                    return continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON,
                                               false, ast_start, shy_ops_start,
                                               shy_ops_pos, (tt2->type.ptrs == 0));
               }
            }
            else if (tt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_DEF)
            {
                cu->ast.head -= sizeof(ast_node);
                t1 = consume_token(cu);
                require_token(cu, t1, TOKEN_PAREN_OPEN);
                ureg pre_params_size = get_ast_size(cu);
                parse_param_list(cu, TOKEN_PAREN_CLOSE);
                add_size_node(cu, pre_params_size);
                ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                n->common.size = get_ast_growth(cu, ast_start);
                n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
                return parse_block(cu);
            }
            else if(tt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_INST){
                cu->ast.head -= sizeof(ast_node);
                t1 = consume_token(cu);
                require_token(cu, t1, TOKEN_PAREN_OPEN);
                if(tt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS){
                    change_param_list_to_arg_list(cu, (ast_node*)cu->ast.head - 2,
                                                  tt2 - (tt2 - 1)->type.size - 1);
                }
                ast_rel_ptr args_siz = parse_arg_list(cu, TOKEN_PAREN_CLOSE) + 1;
                ast_node *e = claim_ast_space(cu, sizeof(ast_node) * 2);
                e->expr.size = args_siz;
                e++;
                e->expr.size = get_ast_growth(cu, ast_start) - type_size - 1;
                e->expr.type = EXPR_NODE_GENERIC_FN_CALL;
                return parse_type_as_expr_begin(cu, ast_start, t, false,
                                            TOKEN_SEMICOLON, TOKEN_SEMICOLON, true);
            }
            else if(tt2->type.type == EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS){
                cu->ast.head -= sizeof(ast_node);
                ureg tt2_pos = get_ast_size(cu) - sizeof(ast_node);
                t1 = consume_token(cu);
                require_token(cu, t1, TOKEN_PAREN_OPEN);
                ureg pre_list_size = get_ast_size(cu);
                arg_or_params_list r = parse_arg_or_param_list(cu);
                t1 = peek_token(cu);
                if(t1->type == TOKEN_BRACE_OPEN){
                    add_size_node(cu, pre_list_size);
                    ast_node *n = (ast_node *) (cu->ast.start + ast_start);
                    n->common.size = get_ast_growth(cu, ast_start);
                    n->common.type = ASTNT_GENERIC_FUNCTION_DECLARATION;
                    return parse_block(cu);
                }
                else{
                    tt2 = (ast_node*)(cu->ast.start + tt2_pos);
                    if(r == AOPL_AMBIGUOUS){
                        change_param_list_to_arg_list(
                            cu, (ast_node*)cu->ast.head - 1, tt2);
                    }
                    change_generic_param_list_to_arg_list(
                        cu, tt2 - 1, tt2 - tt2->type.size);
                    ast_node* n = claim_ast_space(cu, sizeof(ast_node) * 2);
                    n->expr.size = get_ast_growth(cu, pre_list_size)-1;
                    n++;
                    n->expr.type = EXPR_NODE_GENERIC_FN_CALL;
                    n->expr.size = get_ast_growth(cu, ast_start) - type_size - 1;
                    return parse_type_as_expr_begin(cu, ast_start, t, false,
                                            TOKEN_SEMICOLON, TOKEN_SEMICOLON, true);
                }
            }
        }
        else{
            return parse_type_as_expr_begin(cu, ast_start, t, false,
                                            TOKEN_SEMICOLON, TOKEN_SEMICOLON, false);
        }
    }
}
static inline int parse_elem(cunit* cu, token_type end_tok){
    token* t1;
    t1 = peek_token(cu);
    switch(t1->type) {
        case TOKEN_STRING: {
            //can only ever be a type decl with a leading string
            token* t2;
            token* t3;
            t2 = peek_2nd_token(cu);
            if (t2->type == TOKEN_STRING) {
                int mods = parse_modifiers(cu, t1);
                //t2 is invalidated, t1 is pointing to first non modifier token

                if(str_eq_keyword(t1->str, KEYWORD_STRUCT))return parse_struct(cu, mods);
                if(str_eq_keyword(t1->str, KEYWORD_TYPEDEF))return parse_typedef(cu, mods);
                t3 = peek_nth_token(cu, 3);
                if(t3->type == TOKEN_PAREN_OPEN){
                    ureg ast_pos = get_ast_size(cu);
                    claim_ast_space(cu, sizeof(ast_node));
                    parse_type(cu);
                    return parse_function_decl_after_type(cu, mods, ast_pos);
                };
                if(t3->type == TOKEN_BRACE_OPEN){
                    ureg ast_pos = get_ast_size(cu);
                    claim_ast_space(cu, sizeof(ast_node));
                    parse_type(cu);
                    return parse_generic_function_decl_after_type(cu, mods, ast_pos);
                }
                return parse_leading_string(cu); //TODO: implement this branch
            }
            if(str_eq_keyword(t1->str, KEYWORD_CONST) && t2->type == TOKEN_PAREN_OPEN){
                ureg ast_pos = get_ast_size(cu);
                claim_ast_space(cu, sizeof(ast_node));
                parse_type(cu);
                t1 = peek_token(cu);
                if(t1->type == TOKEN_PAREN_OPEN) {
                    return parse_function_decl_after_type(cu, 0, ast_pos);
                }
                if(t1->type == TOKEN_BRACE_OPEN){
                    return parse_generic_function_decl_after_type(cu, 0, ast_pos);
                }
                CIM_ERROR("Unexpected token following type declaration");
            }
           return parse_leading_string(cu);
        }
        case TOKEN_HASH:
        case TOKEN_DOUBLE_HASH: {
            return parse_meta(cu, t1);
        }
        case TOKEN_EOF: {
            void_lookahead_token(cu);
            if(end_tok == TOKEN_EOF) return 1;
            syntax_error(cu, t1, 1, 1,
                         "syntax error: reached end of file before reaching %s",
                         get_token_type_str(cu, end_tok));
        }
        case TOKEN_BRACE_CLOSE:{
            void_lookahead_token(cu);
            if(end_tok == TOKEN_BRACE_CLOSE) return 1;
            syntax_error(cu, t1, 1, 1,
                         "syntax error: reached '}' while not being in a scope",
                         get_token_type_str(cu, end_tok));
        }
        default: {
            return parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false);
        }
    }
}
static void parse_file_scope(cunit* cu){
    while (parse_elem(cu, TOKEN_EOF) == 0);
}
void parse_file(cunit* cu, char* filename){
    tokenizer_open_file(cu, filename);

    parse_file_scope(cu);
    tokenizer_close_file(cu);
}

