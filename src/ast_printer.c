#include "ast_printer.h"
#include <stdio.h>
#include "error.h"
#include "ast.h"
static void print_type(ast_node* t);
static void print_sub_expr(ast_node *e);
void write(char* str){
    fputs(str, stdout);
}
void print_op(u8 op){
	switch(op){
        case OP_UNARY_PLUS:
        case OP_ADD:putchar('+');return;
        case OP_PREINCREMENT:
        case OP_POSTINCREMENT: putchar('+');putchar('+'); return;
        case OP_ADD_ASSIGN: putchar('+');putchar('='); return;
        case OP_UNARY_MINUS:
        case OP_SUBTRACT:putchar('-'); return;
        case OP_PREDECREMENT:
        case OP_POSTDECREMENT: putchar('-');putchar('-'); return;
        case OP_SUBTRACT_ASSIGN:putchar('-');putchar('='); return;
        case OP_LOGICAL_NOT: putchar('!');return;
        case OP_NOT_EQUAL: putchar('!');putchar('='); return;
        case OP_MULTIPLY:
        case OP_DEREFERENCE: putchar('*'); return;
        case OP_MULTIPLY_ASSIGN:putchar('*');putchar('='); return;
        case OP_ASSIGN:putchar('='); return;
        case OP_EQUALS:putchar('=');putchar('='); return;
        case OP_MODULO: putchar('%'); return;
        case OP_MODULO_ASSIGN: putchar('%');putchar('='); return;
        case OP_DIVIDE: putchar('/'); return;
        case OP_DIVIDE_ASSIGN: putchar('/');putchar('='); return;
        case OP_BITWISE_AND: putchar('&'); return;
        case OP_LOGICAL_AND: putchar('&');putchar('&'); return;
        case OP_BITWISE_AND_ASSIGN: putchar('&');putchar('='); return;
        case OP_BITWISE_OR: putchar('|'); return;
        case OP_LOGICAL_OR: putchar('|');putchar('|'); return;
        case OP_BITWISE_OR_ASSIGN: putchar('|');putchar('='); return;
        case OP_BITWISE_NOT: putchar('~'); return;
        case OP_BITWISE_NOT_ASSIGN: putchar('~');putchar('='); return;
        case OP_LESS_THAN: putchar('<'); return;
        case OP_LSHIFT: putchar('<');putchar('<'); return;
        case OP_GREATER_THAN: putchar('>'); return;
        case OP_RSHIFT: putchar('>');putchar('>'); return;
        case OP_ACCESS_MEMBER: putchar('.'); return;
        case OP_DEREFERENCE_ACCESS_MEMBER: putchar('-');putchar('>'); return;
        case OP_LSHIFT_ASSIGN: putchar('<');putchar('<');putchar('=');return;
        case OP_RSHIFT_ASSIGN: putchar('>');putchar('>');putchar('=');return;
        case OP_BITWISE_XOR: putchar('^');return;
        case OP_BITWISE_XOR_ASSIGN: putchar('^');putchar('=');return;
        default:CIM_ERROR("Unknown token");
	}
}
void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
}
void print_ptrs(u8 ptrs){
    for(u8 i = 0;i!=ptrs;i++){
        putchar('*');
    }
}
void reverse_print_func_args(ast_node* elem, ast_node* end){
    if(elem == end)return;
    ast_node* nxt = elem - elem->sub_expr.size;
    if(nxt != end){
        reverse_print_func_args(nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_sub_expr(elem);
}
void reverse_print_type_list(ast_node* start, ast_node* end){
    if(start == end)return;
    ast_node* next = start - start->type.size ;
    if(next != end){
        reverse_print_type_list(next, end);
        putchar(',');putchar(' ');
    }
    print_type(start);
}

void reverse_print_func_params(ast_node* elem, ast_node* end){
    if(elem == end)return;
    elem-=1;
    ast_node* nxt = elem - elem->type.size;
    if(nxt != end){
        reverse_print_func_params(nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_type(elem);
    putchar(' ');
    write((elem+1)->str);
}
static void print_type(ast_node* t){
    if(t->type.type == EXPR_NODE_TYPE_SIMPLE){
        fputs((t-1)->str, stdout);
        print_ptrs(t->type.ptrs);
        return;
    }
    ast_node* last = t - t->type.size  + 1;
    ast_node* tn = t-1;
    switch (t->type.type){
        case EXPR_NODE_TYPE_SCOPED:{
            for(ast_node* i = last ; i != tn; i++){
                fputs(i->str, stdout);
                putchar(':');
            }
            fputs(tn->str, stdout);
            print_ptrs(t->type.ptrs);
        }break;
        case EXPR_NODE_TYPE_GENERIC_STRUCT:{
            fputs(tn->str, stdout);
            putchar('{');
            reverse_print_type_list(t - 2, last-1);
            putchar('}');
            print_ptrs(t->type.ptrs);
        }break;
        case EXPR_NODE_TYPE_SCOPED_GENERIC_STRUCT:{
            ast_node* gen_args_rstart = t - 3;
            ast_node* scopes_end = gen_args_rstart - tn->type.size;
            for(ast_node* i = last; i!= scopes_end; i++){
                fputs(i->str, stdout);
                putchar(':');
            }
            fputs((t-2)->str, stdout);
            putchar('{');
            reverse_print_type_list(gen_args_rstart, scopes_end);
            putchar('}');
            print_ptrs(t->type.ptrs);
        }break;
        case EXPR_NODE_TYPE_FN_PTR:{
            ast_node* args_start = (t-2);
            ast_node* ret = args_start - tn->type.size + 1;
            print_type(ret);
            putchar(' ');
            putchar('(');
            print_ptrs(t->type.ptrs);
            putchar(')');
            putchar('(');
            reverse_print_type_list(args_start, ret);
            putchar(')');
        }break;
        case EXPR_NODE_TYPE_ARRAY:{
            ast_node* expr = t-1;
            print_type(expr - expr->sub_expr.size);
            putchar('[');
            if(expr->sub_expr.size != 1)print_sub_expr(expr-1);
            putchar(']');
            print_ptrs(t->type.ptrs);
        }break;
        default:CIM_ERROR("Unknown AST_TYPE_TYPE");
    }
}
static void print_sub_expr(ast_node *e){
    ast_node* e2 = e-1;
    switch(e->sub_expr.type){
        case EXPR_NODE_NUMBER:
        case EXPR_NODE_VARIABLE:{
            write(e2->str);
        } return;
        case EXPR_NODE_LITERAL:{
            putchar('\"');
            write(e2->str);
            putchar('\"');
        } return;
        case EXPR_NODE_BINARY_LITERAL: {
            putchar('\'');
            write(e2->str);
            putchar('\'');
        }return;
        case EXPR_NODE_OP_L:{
            putchar('(');
            print_op(e->op.opcode);
            print_sub_expr(e2);
            putchar(')');
        }return;
        case EXPR_NODE_OP_R:{
            putchar('(');
            print_sub_expr(e2);
            print_op(e->op.opcode);
            putchar(')');
        }return;
        case EXPR_NODE_OP_LR:{
            putchar('(');
            ast_node* r = e2;
            ast_node* l;
            l= r - r->sub_expr.size;
            print_sub_expr(l);
            putchar(' ');
            print_op(e->op.opcode);
            putchar(' ');
            print_sub_expr(r);
            putchar(')');
        };return;
        case EXPR_NODE_FN_CALL:{
            ast_node* end = e - e->sub_expr.size;
            write(e2->str);
            putchar('(');
            reverse_print_func_args(e-2, end);
            putchar(')');
        }return;
        case EXPR_NODE_GENERIC_FN_CALL:{
            ast_node* end = e - e->sub_expr.size;
            write(e2->str);
            ast_node* params_size = e-2;
            ast_node* generic_args_rstart = params_size - params_size->sub_expr.size;
            putchar('{');
            reverse_print_type_list(generic_args_rstart, end);
            putchar('}');putchar('(');
            reverse_print_func_args(params_size-1, generic_args_rstart);
            putchar(')');
        }return;
        case EXPR_NODE_ARRAY_ACCESS:{
            write(e2->str);
            putchar('[');
            print_sub_expr(e-2);
            putchar(']');
        }return;
        case EXPR_NODE_CANCER_PTRS:{
            putchar('(');
            write((e-2)->str);
            putchar(' ');putchar('*');putchar(' ');
            for(int i = 1; i < e->cancer_ptrs.ptrs; i++){
                putchar('('); putchar('*');
            }
            write(e2->str);
            for(int i = 0; i < e->cancer_ptrs.ptrs; i++){
                putchar(')');
            }
        };return;
        case EXPR_NODE_SCOPED_CANCER_PTRS:{
            putchar('(');
            ast_node* scope = e - e->sub_expr.size;
            while(scope != e-2){
                write(scope->str);putchar(':');
                scope++;
            }
            write((e-1)->str);
            putchar(' '); putchar('*'); putchar(' ');
            for(int i = 1; i < e->cancer_ptrs.ptrs; i++){
                putchar('('); putchar('*');
            }
            write(e2->str);
            for(int i = 0; i < e->cancer_ptrs.ptrs; i++){
                putchar(')');
            }
        }return;
        default:CIM_ERROR("Unknown expression type");
    }
}
void print_ast_within(cunit* cu, ureg indent, ast_node* astn, ast_node* end){
    while(astn!=end){
        print_indent(indent);
        switch(astn->ast_expr.type){
            case ASTNT_VARIABLE_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* name =  decl + decl->ast_expr.size -1;
                astn = name + 1;
                print_type(name-1);
                putchar(' ');
                write(name->str);
                putchar(';');putchar('\n');
            }break;
            case ASTNT_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* fn_name =  decl + decl->ast_expr.size -1;
                ast_node* params_size = fn_name-1;
                ast_node* ret_type = params_size - params_size->ast_expr.size;
                print_type(ret_type);
                putchar(' ');
                write(fn_name->str);
                putchar('(');
                reverse_print_func_params(params_size - 1, ret_type);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = fn_name+1;
                void* block_end = (u8*)block + block->size;
                print_ast_within(cu, indent + 1, block + 1, block_end);
                putchar('}');putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_GENERIC_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* fn_name =  decl + decl->ast_expr.size -1;
                ast_node* params_size = fn_name-1;
                ast_node* generic_params_size = params_size - params_size->ast_expr.size;
                ast_node* ret_type = generic_params_size - generic_params_size->ast_expr.size;
                print_type(ret_type);
                putchar(' ');
                write(fn_name->str);
                putchar('{');
                reverse_print_func_params(generic_params_size - 1, ret_type);
                putchar('}');
                putchar('(');
                reverse_print_func_params(params_size - 1, generic_params_size);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = fn_name+1;
                void* block_end = (u8*)block + block->size;
                print_ast_within(cu, indent + 1, block + 1, block_end);
                putchar('}');putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_EXPRESSION:{
                ast_node* n = astn + astn->sub_expr.size - 1;
                if(n != astn) print_sub_expr(n);
                astn = n + 1;
                puts(";");
            }break;
            case ASTNT_TYPEDEF:{
                astn_typedef* t = (void*)astn;
                write("typedef ");write(t->tgt_type.str);putchar(' ');
                ast_node* tn = (ast_node*)(t+1) + t->size;
                print_type(tn);
                astn = tn + 1;
                putchar(';');
                putchar('\n');
            }break;
            default:CIM_ERROR("Unexpected ASTN");
        }
    }
}
void print_ast(cunit* cu){
    print_ast_within(cu, 0, (void*)cu->ast.start, (void*)cu->ast.head);
}
