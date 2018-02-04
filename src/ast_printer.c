#include "ast_printer.h"
#include <stdio.h>
#include "error.h"
#include "ast.h"
static void print_type(ast_node* t);
static void print_sub_expr(ast_node *e, bool allow_types);
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
        case OP_ADDRESS_OF:
        case OP_BITWISE_AND: putchar('&'); return;
        case OP_LOGICAL_AND: putchar('&');putchar('&'); return;
        case OP_BITWISE_AND_ASSIGN: putchar('&');putchar('='); return;
        case OP_BITWISE_OR: putchar('|'); return;
        case OP_LOGICAL_OR: putchar('|');putchar('|'); return;
        case OP_BITWISE_OR_ASSIGN: putchar('|');putchar('='); return;
        case OP_BITWISE_NOT: putchar('~'); return;
        case OP_BITWISE_NOT_ASSIGN: putchar('~');putchar('='); return;
        case OP_LESS_THAN: putchar('<'); return;
        case OP_LESS_THAN_OR_EQUAL: putchar('<');putchar('='); return;
        case OP_LSHIFT: putchar('<');putchar('<'); return;
        case OP_GREATER_THAN: putchar('>'); return;
        case OP_GREATER_THAN_OR_EQUAL: putchar('>');putchar('='); return;
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
    ast_node* nxt = elem - elem->expr.size;
    if(nxt != end){
        reverse_print_func_args(nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_sub_expr(elem, false);
}
void reverse_print_generic_arg_list(ast_node* start, ast_node* end){
    if(start == end)return;
    ast_node* next = start - start->type.size ;
    if(next != end){
        reverse_print_generic_arg_list(next, end);
        putchar(',');putchar(' ');
    }
    print_sub_expr(start, true);
}
void reverse_print_generic_param_list(ast_node* start, ast_node* end){
    if(start == end)return;
    ast_node* next = start - start->type.size ;
    if(next != end){
        reverse_print_generic_param_list(next, end);
        putchar(',');putchar(' ');
    }
    print_type(start - 2);
    putchar(' ');
    write((start-1)->str);
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
    ast_node* nxt = elem - elem->type.size;
    if(nxt != end){
        reverse_print_func_params(nxt, end);
        putchar(',');putchar(' ');
    }
    print_type(elem-2);
    putchar(' ');
    write((elem-1)->str);
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
            print_type(t - 2);
            putchar(':');
            write(tn->str);
        }return;
        case EXPR_NODE_TYPE_GENERIC_STRUCT_INST:{
            print_type(tn - tn->type.size);
            putchar('{');
            reverse_print_type_list(t - 2,  tn - tn->type.size);
            putchar('}');
            print_ptrs(t->type.ptrs);
        }break;
        case EXPR_NODE_TYPE_GENERIC_STRUCT_DEF:{
        case EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS:
            print_type(tn - tn->type.size);
            putchar('{');
            reverse_print_generic_param_list(t - 2, tn - tn->type.size);
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
            print_type(expr - expr->expr.size);
            putchar('[');
            if(expr->expr.size != 1)print_sub_expr(expr, false);
            putchar(']');
            print_ptrs(t->type.ptrs);
        }break;
        default:CIM_ERROR("Unknown AST_TYPE_TYPE");
    }
}
static void print_sub_expr(ast_node *e, bool allow_types){
    ast_node* e2 = e-1;
    switch(e->expr.type){
        case EXPR_NODE_TYPE_SCOPED:{
            print_type(e - 2);
            putchar(':');
            write(e2->str);
        }return;
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
            print_op(e->expr.special.opcode);
            print_sub_expr(e2, false);
            putchar(')');
        }return;
        case EXPR_NODE_OP_R:{
            putchar('(');
            print_sub_expr(e2, false);
            print_op(e->expr.special.opcode);
            putchar(')');
        }return;
        case EXPR_NODE_OP_LR:{
            putchar('(');
            ast_node* r = e2;
            ast_node* l;
            l= r - r->expr.size;
            print_sub_expr(l, false);
            putchar(' ');
            print_op(e->expr.special.opcode);
            putchar(' ');
            print_sub_expr(r, false);
            putchar(')');
        }return;
        case EXPR_NODE_CAST:{
            putchar('(');
            ast_node* r = e2;
            ast_node* l;
            l= r - r->expr.size;
            putchar('(');
            print_type(l);
            putchar(')');
            print_sub_expr(r, false);
            putchar(')');
        }return;
        case EXPR_NODE_FN_CALL:{
            ast_node* name = e2 - e2->expr.size;
            print_type(name);
            putchar('(');
            reverse_print_func_args(e-2, name);
            putchar(')');
        }return;
        case EXPR_NODE_GENERIC_FN_CALL:{
            ast_node* params_size = e-1;
            ast_node* generic_args_size = params_size - params_size->expr.size;
            ast_node* name = generic_args_size - generic_args_size->expr.size;
            print_type(name);
            putchar('{');
            reverse_print_generic_arg_list(generic_args_size - 1, name);
            putchar('}');putchar('(');
            reverse_print_func_args(params_size-1, generic_args_size);
            putchar(')');
        }return;
        case EXPR_NODE_ARRAY_ACCESS:{
            print_sub_expr(e2 - e2->expr.size, false);
            putchar('[');
            print_sub_expr(e2, false);
            putchar(']');
        }return;
        case EXPR_NODE_CANCER_PTRS:{
            putchar('(');
            print_type(e-2);
            putchar(' ');putchar('*');putchar(' ');
            for(int i = 1; i < e->expr.special.ptrs; i++){
                putchar('('); putchar('*');
            }
            write(e2->str);
            for(int i = 0; i < e->expr.special.ptrs; i++){
                putchar(')');
            }
        };return;
        default:{
            if(allow_types){
                print_type(e);
            }
            else{
                 CIM_ERROR("Unknown expression type");
            }
        }
    }
}
void print_ast_within(cunit* cu, ureg indent, ast_node* astn, ast_node* end, bool trailing_nl){
    while(astn!=end){
        print_indent(indent);
        switch(astn->common.type){
            case ASTNT_VARIABLE_DECLARATION:
            case ASTNT_VARIABLE_DECLARATION_AMBIGUOUS:{
                ast_node* decl = (void*)astn;
                if(astn->var_decl.assigning == true){
                    ast_node* expr =  decl + decl->common.size -1;
                    astn = expr + 1;
                    ast_node* name = expr - expr->expr.size;
                    print_type(name - name->type.size);
                    putchar(' ');
                    print_type(name);
                    putchar(' ');putchar('=');putchar(' ');
                    print_sub_expr(expr, false);
                    putchar(';');
                    if(trailing_nl)putchar('\n');
                }
                else{
                    ast_node* name =  decl + decl->common.size -1;
                    astn = name + 1;
                    print_type(name - name->type.size);
                    putchar(' ');
                    print_type(name);
                    putchar(';');
                    if(trailing_nl)putchar('\n');
                }

            }break;
            case ASTNT_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* params_size =  decl + decl->common.size - 1;
                ast_node* fn_name = params_size - params_size->common.size;
                ast_node* ret_type = fn_name - fn_name->common.size;
                print_type(ret_type);
                putchar(' ');
                print_type(fn_name);
                putchar('(');
                reverse_print_func_params(params_size - 1, fn_name);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = params_size+1;
                void* block_end = (u8*)block + block->full_size;
                print_ast_within(cu, indent + 1, block + 1, block_end, true);
                putchar('}');
                if(trailing_nl)putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_GENERIC_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* params_size =  decl + decl->common.size - 1;
                ast_node* generic_params_size = params_size - params_size->common.size;
                ast_node* fn_name = generic_params_size - generic_params_size->common.size;
                print_type(fn_name - fn_name->common.size);
                putchar(' ');
                print_type(fn_name);

                putchar('{');
                reverse_print_func_params(generic_params_size - 1, fn_name);
                putchar('}');
                putchar('(');
                reverse_print_func_params(params_size - 1, generic_params_size);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = params_size+1;
                void* block_end = (u8*)block + block->full_size;
                print_ast_within(cu, indent + 1, block + 1, block_end, true);
                putchar('}');putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_EXPRESSION:{
                ast_node* n = astn + astn->expr.size - 1;
                if(n != astn) print_sub_expr(n, false);
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
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_FOR:{
                write("for(");
                ast_node* st = astn + 1;
                print_ast_within(cu,0,st,  st + st->expr.size, false);
                st+= st->expr.size;
                ast_node* st_end = astn + astn->common.size;
                putchar(' ');
                st+= st->expr.size;
                print_sub_expr(st-1, false);
                while(st != st_end - 1){
                    putchar(';');putchar(' ');
                    st+= st->expr.size;
                    print_sub_expr(st-1, false);
                }
                putchar(')');putchar('{');putchar('\n');
                astn = (ast_node*)((u8*)st_end + st_end->full_size);
                print_ast_within(cu, indent +1 , st_end + 1, astn, true);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            default:CIM_ERROR("Unexpected ASTN");
        }
    }
}
void print_ast(cunit* cu){
    print_ast_within(cu, 0, (void*)cu->ast.start, (void*)cu->ast.head, true);
}
