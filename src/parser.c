#include "parser.h"
#include <stdio.h>
#include "ast.h"
#include <assert.h>
#include "error.h"
#include "keywords.h"
static inline int parse_elem(cunit* cu);
static int parse_expr(cunit *cu, token_type term1, token_type term2, bool sub_expr);

#define OP_RANGE (1 << (sizeof(u8)*8))
static u8 prec_table[OP_RANGE];
static u8 assoc_table[OP_RANGE];

enum assocs{
    LEFT_ASSOCIATIVE = 0,
    RIGHT_ASSOCIATIVE = 1,
};

static u8 prec_table [OP_RANGE] = {
    [OP_NONE] = 1, //initialization flag
    //uniquely has this precedence level so it is not removed during flush
    [OP_TEMP_PAREN_OPEN] = 0,

    [OP_POSTINCREMENT] = 15,
    [OP_POSTINCREMENT] = 15,

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
    [OP_NONE] = RIGHT_ASSOCIATIVE,
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
    dbuffer_init(&cu->shy_ops);
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
void cunit_fin(cunit* cu){
   	sbuffer_fin(&cu->data_store);
	dbuffer_fin(&cu->string_ptrs);
	dbuffer_fin(&cu->ast);
    dbuffer_fin(&cu->shy_ops);
}
static inline ast_rel_ptr get_ast_growth(cunit* cu, ureg ast_start){
    return (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - ast_start) / sizeof(ast_node));
}
static inline void flush_shy_op(cunit* cu, ast_node* s){
    ast_node* expr_rit =  (void*)(cu->ast.head - sizeof(ast_node));
    int its;
    switch (s->sub_expr.type){
        case EXPR_NODE_TYPE_PAREN: its = 0;break;
        case EXPR_NODE_TYPE_OP_LR: its = 2;break;
        default: its = 1; break;
    }
    for(int i = 0; i != its; i++){
        expr_rit = expr_rit - expr_rit->sub_expr.size;
    }
    //needs to be precomputed because the realloc might invalidate the expr_rit ptr
    ast_rel_ptr size = (ast_rel_ptr)((ast_node*)cu->ast.head - expr_rit);
    ast_node* e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    e->op.node_type = s->op.node_type;
    e->op.opcode = s->op.opcode;
    //TODO: evaluate the necessity of this for single arg ops
    e->op.size = size;
    //this will hopefully be inlined and brought out of the loop
    cu->shy_ops.head -= sizeof(ast_node);
}
static inline void push_shy_op(cunit* cu, ast_node* sop, ast_node** sho_ri, ast_node** sho_re, ureg shy_ops_start){
    if(dbuffer_has_space(&cu->shy_ops, sizeof(ast_node))){
        *((ast_node*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(ast_node);
        (*sho_ri)++;
    }
    else{
        dbuffer_grow(&cu->shy_ops);
        *((ast_node*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(ast_node);
        *sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(ast_node));
        *sho_ri = (void*)(cu->shy_ops.head - sizeof(ast_node));
    }
}
static inline ast_rel_ptr parse_arg_list(cunit* cu, token_type end_tok){
    ureg ast_pos = dbuffer_get_size(&cu->ast);
    token t;
    peek_token(cu, &t);
    if(t.type == end_tok){
        void_lookahead_token(cu);
        return 0;
    }
    while (parse_expr(cu, TOKEN_COMMA, end_tok, true) == 0);
    return (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - ast_pos) / sizeof(ast_node));
}
static int continue_parse_expr(cunit* cu, token_type term1, token_type term2, bool sub_expr,
                               ureg expr_start, ureg shy_ops_start,ureg shy_op_pos, token t1, bool expecting_op)
{
    token t2;
    ast_node* e;
    ast_node sop;
    u8 prec;
    ast_node* sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(ast_node));
    ast_node* sho_ri = (void*)(cu->shy_ops.start + shy_op_pos - sizeof(ast_node));
    ureg open_paren_count = 0;
    while(true){
        switch(t1.type){
            case TOKEN_DOUBLE_PLUS: {
                sop.op.opcode = (expecting_op) ? OP_POSTINCREMENT : OP_PREINCREMENT;
            }goto lbl_op_l_or_r;
            case TOKEN_DOUBLE_MINUS:{
                sop.op.opcode = (expecting_op) ? OP_POSTDECREMENT : OP_PREDECREMENT;
            }//fallthrough to op_l_or_r
            lbl_op_l_or_r:{
                sop.sub_expr.type = (expecting_op) ? EXPR_NODE_TYPE_OP_R : EXPR_NODE_TYPE_OP_L;
                prec = prec_table[sop.op.opcode];
                if (assoc_table[sop.op.opcode] == LEFT_ASSOCIATIVE) {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op.opcode] >= prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                else {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op.opcode] > prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                //expecting op stays the same
            }break;
            case TOKEN_STAR:{
                if (expecting_op) {
                    sop.op.opcode = OP_MULTIPLY;
                    goto lbl_op_lr;
                }
                sop.op.opcode = OP_DEREFERENCE;
            }goto lbl_op_unary;
            case TOKEN_AND:{
                if (expecting_op) {
                    sop.op.opcode = OP_BITWISE_AND;
                    goto lbl_op_lr;
                }
                sop.op.opcode = OP_ADDRESS_OF;
            }goto lbl_op_unary;
            case TOKEN_PLUS: {
                if (expecting_op) {
                    sop.op.opcode = OP_ADD;
                    goto lbl_op_lr;
                }
                sop.op.opcode = OP_UNARY_PLUS;
            }goto lbl_op_unary;
            case TOKEN_MINUS:{
                if (expecting_op) {
                    sop.op.opcode = OP_SUBTRACT;
                    goto lbl_op_lr;
                }
                sop.op.opcode = OP_UNARY_MINUS;
            }//fallthrough to lbl_op_unary
            lbl_op_unary: {
                sop.sub_expr.type = EXPR_NODE_TYPE_UNARY;
                prec = prec_table[sop.op.opcode];
                //unary is always right associative
                for (; sho_ri != sho_re && prec_table[sho_ri->op.opcode] > prec; sho_ri--) {
                    flush_shy_op(cu, sho_ri);
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
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
                sop.op.opcode = (u8)(t1.type);
            }//fall through to op_lr
            lbl_op_lr: {
                sop.sub_expr.type = EXPR_NODE_TYPE_OP_LR;
                prec = prec_table[sop.op.opcode];
                if (assoc_table[sop.op.opcode] == LEFT_ASSOCIATIVE) {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op.opcode] >= prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                else {
                    for (; sho_ri != sho_re && prec_table[sho_ri->op.opcode] > prec; sho_ri--) {
                        flush_shy_op(cu, sho_ri);
                    }
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_OPEN: {
                open_paren_count++;
                sop.sub_expr.type = EXPR_NODE_TYPE_PAREN;
                sop.op.opcode = OP_TEMP_PAREN_OPEN;
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
            }break;
            case TOKEN_PAREN_CLOSE: {
                if(open_paren_count==0)goto lbl_default;
                open_paren_count--;
                for (;sho_ri != sho_re && sho_ri->op.opcode != OP_TEMP_PAREN_OPEN;
                      sho_ri--)
                {
                    flush_shy_op(cu, sho_ri);
                }
                //removing the OP_TEMP_PAREN_OPEN
                dbuffer_pop_back(&cu->shy_ops, sizeof(ast_node));
                sho_ri--;
                expecting_op = true;
            }break;
            case TOKEN_NUMBER:
            case TOKEN_LITERAL:
            case TOKEN_BINARY_LITERAL:
            {
                assert(!expecting_op);
                e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
                e->str = t1.str;
                e++;
                e->sub_expr.size = 2;
                e->sub_expr.type= (expr_node_type)t1.type;
                expecting_op = true;
            }break;
            case TOKEN_STRING: {
                assert(!expecting_op);
                peek_token(cu, &t2);
                if (t2.type == TOKEN_PAREN_OPEN) {
                    void_lookahead_token(cu);
                    char* fn_name = t1.str;
                    ureg ast_size = dbuffer_get_size(&cu->ast);
                    parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                    e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
                    e->str = fn_name;
                    e++;
                    e->sub_expr.size = (ast_rel_ptr)
                            ((dbuffer_get_size(&cu->ast) - ast_size) / sizeof(ast_node));
                    e->sub_expr.type= EXPR_NODE_TYPE_FN_CALL;
                }
                else if(t2.type == TOKEN_BRACE_OPEN){
                    void_lookahead_token(cu);
                    char* el_name = t1.str;
                    //we cant' just use a pointer here because we might have a realloc
                    //this ammends the - sizeof(ast_node) because its used in a subtraction
                    //so it's gonna be canceled out
                    ureg el_end= dbuffer_get_size(&cu->ast);
                    parse_arg_list(cu, TOKEN_BRACE_CLOSE);
                    peek_token(cu, &t2);
                    if(t2.type == TOKEN_PAREN_OPEN){
                        void_lookahead_token(cu);
                        ureg generic_args_rstart = dbuffer_get_size(&cu->ast);
                        parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                        ast_rel_ptr arg_list_size = (ast_rel_ptr)
                                ((dbuffer_get_size(&cu->ast) - generic_args_rstart) /
                                sizeof(ast_node));
                        e = dbuffer_claim_small_space(&cu->ast, sizeof(*e) * 3);
                        e->sub_expr.size = arg_list_size;
                        e++;
                        e->str = el_name;
                        e++;
                        e->sub_expr.size= (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - el_end) / sizeof(ast_node));
                        e->sub_expr.type= EXPR_NODE_TYPE_GENERIC_FN_CALL;
                    }
                    else{
                       //scoped type stuff
                    }
                }
                else if(t2.type == TOKEN_BRACKET_OPEN){
                    ureg el_end= dbuffer_get_size(&cu->ast);
                    parse_arg_list(cu, TOKEN_BRACKET_CLOSE);
                    e = dbuffer_claim_small_space(&cu->ast, sizeof(*e) * 2);
                    e->str= t1.str;
                    e++;
                    e->sub_expr.size= (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - el_end) / sizeof(ast_node));
                    e->sub_expr.type= EXPR_NODE_TYPE_ARRAY_ACCESS;
                }
                else {
                    e = dbuffer_claim_small_space(&cu->ast, sizeof(*e) * 2);
                    e->str= t1.str;
                    e++;
                    e->sub_expr.size = 2;
                    e->sub_expr.type= EXPR_NODE_TYPE_VARIABLE;
                }
                //true for all: fn call, var, array access and generic fn call
                expecting_op = true;
            }break;
            case TOKEN_EOF:CIM_ERROR("Unexpected eof");
            default:{
lbl_default:
                if(t1.type == term1 || t1.type == term2){
                    for(;sho_ri != sho_re; sho_ri--){
                        flush_shy_op(cu, sho_ri);
                    }
                    ast_node* expr;
                    if(sub_expr){
                        expr = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
                        expr->sub_expr.size= (ast_rel_ptr)((dbuffer_get_size(&cu->ast) - expr_start) / sizeof(ast_node));
                        expr->sub_expr.type = EXPR_NODE_TYPE_EXPR;
                    }
                    else{
                        expr = (ast_node*)(cu->ast.start + expr_start);
                        expr->ast_expr.size = (ast_rel_ptr)((ast_node*)cu->ast.head - expr);
                        expr->ast_expr.type = ASTNT_EXPRESSION;
                    }
                    return (t1.type == term1) ? 0 : 1;
                }
                CIM_ERROR("Unexpected token");
            }
        }
        consume_token(cu, &t1);
    }
}
static int parse_expr(cunit *cu, token_type term1, token_type term2, bool sub_expr){
    token t1;
    token t2;
    consume_token(cu, &t1);
    if(t1.type == term1 || t1.type == term2)return 0;
    peek_token(cu, &t2);
    //printf("parsing expr (%llu)\n", POS(cu->ast.head));
    if(t2.type == term1 || t2.type == term2){
        void_lookahead_token(cu);
        //short expression optimization
        ast_node* e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
        (e+1)->sub_expr.size = 2;
        if( t1.type == TOKEN_NUMBER ||
            t1.type == TOKEN_BINARY_LITERAL ||
            t1.type == TOKEN_LITERAL)
        {
            //reverse order for subexprs
            if(sub_expr){
                e->str = t1.str;
                e++;
                e->sub_expr.type = (expr_node_type)t1.type; //token type matches expr elem type for these
            }
            else{
                e->sub_expr.type = (expr_node_type)t1.type; //token type matches ASTNT type for these
                e++;
                e->str = t1.str;
            }
        }
        else if(t1.type == TOKEN_STRING) {
            if(sub_expr){
                e->str = t1.str;
                e++;
                e->sub_expr.type = EXPR_NODE_TYPE_VARIABLE;
            }
            else{
                e->sub_expr.type= EXPR_NODE_TYPE_VARIABLE;
                e++;
                e->str = t1.str;
            }
        }
        else{
            CIM_ERROR("Unexpected Token");
        }
        return (t2.type == term1) ? 0 : 1;
    }
    ureg expr_start = dbuffer_get_size(&cu->ast);
    if(!sub_expr)dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    ureg shy_ops_start = cu->shy_ops.head - cu->shy_ops.start;
    continue_parse_expr(cu, term1, term2, sub_expr, expr_start, shy_ops_start,shy_ops_start, t1, false);
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
    ureg ast_pos = dbuffer_get_size(&cu->ast);
    ast_node* t;
    ast_node* tn;
    token t1;
    token t2;
    consume_token(cu, &t1);
    peek_token(cu, &t2);
    bool scoped= false;
    if(t2.type == TOKEN_COLON){
        scoped = true;
        do{
            tn = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
            tn->str = t1.str;
            void_lookahead_token(cu);
            consume_token(cu, &t1);
            peek_token(cu, &t2);
        }while(t2.type == TOKEN_COLON);
        //t2 is peeked during the loop
    }
    if(t2.type == TOKEN_BRACE_OPEN){
        ureg post_scope_ast_pos = dbuffer_get_size(&cu->ast);
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
        scoped_str = dbuffer_claim_small_space(
            &cu->ast, sizeof(ast_node) * (2 + scoped));
        tn = scoped_str + scoped;
        t = (void*)(tn + 1);
        ureg final_ast_pos = dbuffer_get_size(&cu->ast);
        t->type.size = (ast_rel_ptr)((final_ast_pos - ast_pos) / sizeof(ast_node));
        t->type.type = (scoped) ? AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT :
                             AST_TYPE_TYPE_GENERIC_STRUCT;
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
        tn = dbuffer_claim_small_space(
        &cu->ast, sizeof(ast_node) * 2);
        t = (void*)(tn + 1);
        t->type.size  = (ast_rel_ptr)
                ((dbuffer_get_size(&cu->ast) - ast_pos) / sizeof(ast_node));
        tn->str = t1.str;
        // simple type
        t->type.type = scoped ? AST_TYPE_TYPE_SCOPED : AST_TYPE_TYPE_SIMPLE;
        t->type.ptrs = count_ptrs(cu);
        peek_token(cu, &t2);//t2 peek is restored
    }
    //loop because a function pointer might be the return type of a function pointer
    while(t2.type == TOKEN_PAREN_OPEN){
        peek_2nd_token(cu, &t1);
        if(t1.type != TOKEN_STAR)break;
        // function pointer
        ast_rel_ptr ret_type_size = (ast_rel_ptr)
                ((dbuffer_get_size(&cu->ast) - ast_pos) / sizeof(ast_node));
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
        tn = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
        t = tn+1;
        t->type.size = (ast_rel_ptr)
                    (dbuffer_get_size(&cu->ast) - ast_pos) / sizeof(ast_node);
        t->type.type = AST_TYPE_TYPE_FN_PTR;
        t->type.ptrs = ptrs;
        tn->type.size = t->type.size - ret_type_size - 2;
        //doesnt have a name, not part of the type
        peek_token(cu, &t2);
    }
    while(t2.type == TOKEN_BRACKET_OPEN){
        void_lookahead_token(cu);
        peek_token(cu, &t2);
        ureg expr_start = dbuffer_get_size(&cu->ast);
        parse_expr(cu, TOKEN_BRACKET_CLOSE, TOKEN_BRACKET_CLOSE, true);
        ast_rel_ptr exp_size = get_ast_growth(cu, expr_start);;
        t = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
        t->type.size = exp_size;
        t++;
        t->type.type = AST_TYPE_TYPE_ARRAY;
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
    ureg ast_start = dbuffer_get_size(&cu->ast);
    dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    consume_token(cu, &t1);
    CIM_ASSERT(t1.type == TOKEN_BRACE_OPEN);
    int r;
    do{
      r = parse_elem(cu);
    } while(r == 0);
    CIM_ASSERT(r == 2);
    ast_node* n = (ast_node*)(cu->ast.start + ast_start);
    n->size = dbuffer_get_size(&cu->ast) - ast_start;
    return 0;
}
static int parse_typedef(cunit* cu, int mods){
    //it is safe to void 1 lookahead, as KEYWORD_TYPEDEF must aways be looked ahead
    void_lookahead_token(cu);
    ureg ast_pos = dbuffer_get_size(&cu->ast);
    astn_typedef* td = dbuffer_claim_small_space(&cu->ast, sizeof(astn_typedef));
    td->type = ASTNT_TYPEDEF;
    token t;
    consume_token(cu, &t);
    td->tgt_type.str = t.str;
    parse_type(cu);
    td = (void*)(cu->ast.start + ast_pos);
    td->size = (ast_rel_ptr)
            ((dbuffer_get_size(&cu->ast) - ast_pos - sizeof(astn_typedef))
             / sizeof(ast_node) - 1);
    consume_token(cu, &t);
    CIM_ASSERT(t.type == TOKEN_SEMICOLON);
    return 0;
}
static int check_size_inc_from_type_as_expr(ast_node* type){
    //TODO
}
static int parse_type_as_expr_begin(cunit *cu, ureg ast_start, ast_node *type,
                                    token_type term1, token_type term2, bool after_fn_call){
    //TODO: add scoped
    ureg shy_ops_start = dbuffer_get_size(&cu->shy_ops);
    ureg shy_ops_pos;
    //type type is identical to sub_expr_type var / scoped var
    if(type->type.ptrs){
        ast_node* sop = dbuffer_claim_small_space(&cu->shy_ops, sizeof(ast_node) * type->type.ptrs);
        sop->op.opcode = OP_MULTIPLY;
        sop->op.node_type = EXPR_NODE_TYPE_OP_LR;
        sop++;
        while((void*)sop!= cu->shy_ops.head){
            sop->op.node_type = EXPR_NODE_TYPE_UNARY;
            sop->op.opcode = OP_DEREFERENCE;
        }
        shy_ops_pos = dbuffer_get_size(&cu->shy_ops);
    }
    else{
        shy_ops_pos = shy_ops_start;
    }
    token t1;
    consume_token(cu, &t1);
    return continue_parse_expr(cu, term1, term2, false, ast_start,
                               shy_ops_start,shy_ops_pos, t1,
                               (after_fn_call == true || type->type.ptrs == 0));
};
static void turn_param_list_into_arg_list(cunit* cu, ast_node* tgt, ast_node* rstart, ast_node* rend){
    //TODO
}
//returns 0 if it's ambiguous, 1 if it created a params list and 2 for a argument list
typedef enum arg_or_params_list_e{
    AOPL_ARG_LIST,
    AOPL_PARAM_LIST,
    AOPL_AMBIGUOUS, //in this case it was parsed as a param list
}arg_or_params_list;
static arg_or_params_list parse_arg_or_param_list(cunit* cu, bool generic, ast_rel_ptr* r_size_inc_as_arg_list){
    ureg ast_start = dbuffer_get_size(&cu->ast);
    token t1;
    token t2;
    token_type term;
    consume_token(cu, &t1);
    if(generic){
        CIM_ASSERT(t1.type == TOKEN_BRACE_OPEN);
        term = TOKEN_BRACE_CLOSE;
    }
    else{
        CIM_ASSERT(t1.type == TOKEN_PAREN_OPEN);
        term = TOKEN_PAREN_CLOSE;
    }
    peek_token(cu, &t1);
    ast_rel_ptr size_inc_as_arg_list = 0;
    ast_node* t;
    while(t1.type != term){
        t = parse_type(cu);
        if(t->type.type == AST_TYPE_TYPE_SIMPLE || t->type.type == AST_TYPE_TYPE_SCOPED) {
            peek_token(cu, &t1);
            if (t1.type == term){
                goto its_an_arg_list;
            }
            else if(t->type.ptrs == 0){
                if(t1.type == TOKEN_STRING) goto its_a_param_list;
                goto its_an_arg_list;
            }
            else{
                peek_2nd_token(cu, &t2);
                if( t1.type != TOKEN_STRING ||
                   (t2.type != TOKEN_COMMA && t2.type != term)) goto its_an_arg_list;
                //it's ambiguous, assume param list (much more likely
                //as otherwise it's a thrown away expression) and continue
                size_inc_as_arg_list+=  2 + t->type.ptrs;
                void_2_lookahead_tokens(cu);
                ast_node* n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
                n->str = t1.str;
            }
        }
        else{
            goto its_a_param_list;
        }
    }
    *r_size_inc_as_arg_list = size_inc_as_arg_list;
    return AOPL_AMBIGUOUS;
its_a_param_list:;
    //handling the current parameter
    ast_node* n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    consume_token(cu, &t1);
    n->str = t1.str;
    consume_token(cu, &t1);
    while(t1.type == TOKEN_COMMA){
        parse_type(cu);
        n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
        consume_token(cu, &t1);
        CIM_ASSERT(t1.type == TOKEN_STRING);
        n->str = t1.str;
        consume_token(cu, &t1);
    }
    CIM_ASSERT(t1.type == term);
    ast_rel_ptr params_siz = get_ast_growth(cu, ast_start);
    n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    n->sub_expr.size = params_siz;
    return AOPL_PARAM_LIST;
its_an_arg_list:;
    if(size_inc_as_arg_list != 0){
        ureg ast_pos = dbuffer_get_size(&cu->ast);
        ureg list_size = (ast_pos - ast_start) / sizeof(ast_node);
        ureg preparsed_type_size = t->type.size;
        list_size -= preparsed_type_size;
        //we only need it to be available for our temp_list, we don't actually claim it
        dbuffer_make_space(&cu->ast, (list_size + size_inc_as_arg_list) * sizeof(ast_node));
        ast_node* list = (void*)(cu->ast.head+ast_start);
        ast_node* temp_list = list + list_size + size_inc_as_arg_list;
        memcpy(temp_list, list, (list_size + preparsed_type_size) * (sizeof(ast_node)));
        turn_param_list_into_arg_list(cu, list, temp_list + list_size, temp_list);
        //restore the preparsed type at the end
        t = list + list_size + size_inc_as_arg_list;
        if(t1.type != TOKEN_COMMA && t1.type != term) {
            memcpy(t+1, temp_list + list_size, preparsed_type_size);
            parse_type_as_expr_begin(cu, ast_start, t, TOKEN_COMMA, term, false);
        }
        else{
             memcpy(t, temp_list + list_size, preparsed_type_size);
        }
        cu->ast.head += size_inc_as_arg_list * sizeof(ast_node);
    }
    else{
        if(t1.type != TOKEN_COMMA && t1.type != term){
            memcpy(t+1,t,t->type.size * sizeof(ast_node));
            parse_type_as_expr_begin(cu, ast_start, t, TOKEN_COMMA, term, false);
        }
        else{
            void_lookahead_token(cu);
        }
    }
    if(t1.type != term){
         while(parse_expr(cu, TOKEN_COMMA, term, true) == 0);
    }
    return AOPL_ARG_LIST;
}

static int parse_function_decl_after_type(cunit *cu, int mods, ureg ast_start){
    token t1, t2;
    consume_token(cu, &t1);
    consume_token(cu, &t2);
    CIM_ASSERT(t2.type == TOKEN_PAREN_OPEN);
    ureg ast_pos_pre_params = dbuffer_get_size(&cu->ast);
    peek_token(cu, &t2);
    if(t2.type != TOKEN_PAREN_CLOSE){
        do{
            parse_type(cu);
            ast_node* n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
            consume_token(cu, &t2);
            CIM_ASSERT(t2.type == TOKEN_STRING);
            n->str = t2.str;
            consume_token(cu, &t2);
        }while(t2.type == TOKEN_COMMA);
        CIM_ASSERT(t2.type == TOKEN_PAREN_CLOSE);
    }
    else{
        void_lookahead_token(cu);
    }
    ast_node* s = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
    s->ast_expr.size = get_ast_growth(cu, ast_pos_pre_params) - 2;
    s++;
    s->str = t1.str;
    s = (ast_node*)(cu->ast.start + ast_start);
    s->ast_expr.type = ASTNT_FUNCTION_DECLARATION;
    s->ast_expr.size = get_ast_growth(cu, ast_start);
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
    ureg ast_start = dbuffer_get_size(&cu->ast);
    dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
    ast_node* t = parse_type(cu);
    ureg type_rstart = dbuffer_get_size(&cu->ast)-sizeof(ast_node);
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
        n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
        n->str = t1.str;
        return 0;
    }
    switch(t->type.type) {
        case AST_TYPE_TYPE_SIMPLE: {
            if (t1.type == TOKEN_PAREN_OPEN && t->type.ptrs == 0) {
                //function call
                void_lookahead_token(cu);
                char *fn_name = (t - 1)->str;
                cu->ast.head = (void *) (t - 1); //we have to override these nodes :(
                ast_rel_ptr args_size = parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                ast_node *e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
                e->str = fn_name;
                e++;
                e->sub_expr.size = args_size + 2;
                e->sub_expr.type = EXPR_NODE_TYPE_FN_CALL;
                consume_token(cu, &t1);
                ureg shy_ops_pos = dbuffer_get_size(&cu->shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start, shy_ops_pos,shy_ops_pos, t1, true);
                return 0;
            }
            else if(t1.type == TOKEN_STRING){
                if(t2.type == TOKEN_PAREN_OPEN){
                    char* fn_name = t1.str;
                    if(t->type.ptrs == 0) return parse_function_decl_after_type(cu, 0, ast_start);
                    void_lookahead_token(cu);
                    ast_rel_ptr size_inc_as_arg_list;
                    arg_or_params_list r = parse_arg_or_param_list(cu, false, &size_inc_as_arg_list);
                    if(r == AOPL_AMBIGUOUS){
                        peek_token(cu, &t1);
                        if(t1.type == TOKEN_BRACE_OPEN){
                            r = AOPL_PARAM_LIST;
                        }
                        else{
                            r = AOPL_ARG_LIST;
                            ureg sdiff = dbuffer_get_size(&cu->ast) - ast_start;
                            ureg sreq = sdiff + size_inc_as_arg_list * sizeof(ast_node);
                            dbuffer_make_space(&cu->ast, sreq);
                            ast_node* al_start = (ast_node*)(cu->ast.start + ast_start);
                            ast_node* temp_start = (void*)((u8*)al_start + sreq);
                            memcpy(temp_start, al_start, sdiff);
                            turn_param_list_into_arg_list(cu, al_start,
                                                          (ast_node*)cu->ast.head -1,
                                                          temp_start);
                            cu->ast.head += size_inc_as_arg_list * sizeof(ast_node);
                        }
                    }
                    if(r == AOPL_PARAM_LIST){
                        ast_node* n = (void*)(cu->ast.start + ast_start);
                        n->ast_expr.type = ASTNT_FUNCTION_DECLARATION;
                        n->ast_expr.size = get_ast_growth(cu, ast_start) + 1;
                        n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
                        n->str = fn_name;
                        return parse_block(cu);
                    }
                    else {
                        ast_node* n = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 2);
                        n->str = fn_name;
                        n++;
                        n->sub_expr.type = EXPR_NODE_TYPE_FN_CALL;
                        t = (void*)(cu->ast.start + type_rstart);
                        n->sub_expr.size = get_ast_growth(cu, ast_start) - t->type.size-1;
                        return parse_type_as_expr_begin(cu, ast_start, t,
                                                 TOKEN_SEMICOLON, TOKEN_SEMICOLON,true);
                    }
                }
                else if (t2.type == TOKEN_BRACE_OPEN){
                    //generic function call or decl
                }
            }
            //expression
            parse_type_as_expr_begin(cu, ast_start, t, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false);
        }
        case AST_TYPE_TYPE_GENERIC_STRUCT: {
            if (t1.type == TOKEN_PAREN_OPEN) {
                //generic function call
                void_lookahead_token(cu);
                char *fn_name = (t - 1)->str;
                cu->ast.head = (void *) (t - 1); //we have to override these nodes :(
                ast_rel_ptr args_size = parse_arg_list(cu, TOKEN_PAREN_CLOSE);
                ast_node *e = dbuffer_claim_small_space(&cu->ast, sizeof(ast_node) * 3);
                e->sub_expr.size = args_size;
                e++;
                e->str = fn_name;
                e++;
                e->sub_expr.size = get_ast_growth(cu, ast_start) - 1;//the one belongs to expr
                e->sub_expr.type = EXPR_NODE_TYPE_GENERIC_FN_CALL;
                consume_token(cu, &t1);
                ureg shy_ops_start = dbuffer_get_size(&cu->shy_ops);
                continue_parse_expr(cu, TOKEN_SEMICOLON, TOKEN_SEMICOLON, false,
                                    ast_start,shy_ops_start , shy_ops_start,t1, true);
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
                    ureg ast_pos = dbuffer_get_size(&cu->ast);
                    dbuffer_claim_small_space(&cu->ast, sizeof(ast_node));
                    parse_type(cu);
                    return parse_function_decl_after_type(cu, mods, ast_pos);
                };
                return parse_leading_string(cu); //TODO: implement this branch
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
void parse(cunit* cu, char* str){
    cu->str = str;
    cu->pos = str;
    parse_file_scope(cu);
}
