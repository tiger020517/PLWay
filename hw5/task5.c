#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#define T_NUM 0
#define T_ADD 1
#define T_SUB 2
#define T_ID  3
#define T_FUN 4
#define T_APP 5
#define T_IF  6   
#define T_REC 9  

#define V_NUM 0
#define V_CLOSURE 1

typedef struct Expr Expr;
typedef struct Value Value;
typedef struct Env Env;
typedef struct Thunk Thunk; 

struct Expr {
    int type;
    int n;
    char* name;      
    struct Expr* e1; 
    struct Expr* e2; 
    struct Expr* e3; 
};

struct Thunk {
    Expr* expr;
    Env* ds;
    Value* cache; 
};

struct Env {
    char* name;
    Thunk* thunk; 
    struct Env* next;
};

struct Value {
    int type;
    int n;
    char* param;
    Expr* body;
    Env* ds;
};

//before one
Expr* new_expr(int type) {
    Expr* e = (Expr*)calloc(1, sizeof(Expr));
    e->type = type;
    return e;
}

//before one
Expr* make_num(int n) {
    Expr* e = new_expr(T_NUM);
    e->n = n;
    return e;
}

//before one
Expr* make_add(Expr* l, Expr* r) {
    Expr* e = new_expr(T_ADD);
    e->e1 = l; e->e2 = r;
    return e;
}

//before one
Expr* make_sub(Expr* l, Expr* r) {
    Expr* e = new_expr(T_SUB);
    e->e1 = l; e->e2 = r;
    return e;
}

//before one
Expr* make_id(char* name) {
    Expr* e = new_expr(T_ID);
    e->name = strdup(name);
    return e;
}

//before one
Expr* make_fun(char* param, Expr* body) {
    Expr* e = new_expr(T_FUN);
    e->name = strdup(param);
    e->e1 = body;
    return e;
}

//before one
Expr* make_app(Expr* ftn, Expr* arg) {
    Expr* e = new_expr(T_APP);
    e->e1 = ftn; e->e2 = arg;
    return e;
}

//before one
Expr* make_if(Expr* cond, Expr* then_e, Expr* else_e) {
    Expr* e = new_expr(T_IF);
    e->e1 = cond; e->e2 = then_e; e->e3 = else_e;
    return e;
}

//before one
Expr* make_rec(char* name, Expr* named_expr, Expr* body) {
    Expr* e = new_expr(T_REC);
    e->name = strdup(name);
    e->e1 = named_expr; 
    e->e2 = body;       
    return e;
}

//before one
Value* make_num_val(int n) {
    Value* v = (Value*)calloc(1, sizeof(Value));
    v->type = V_NUM;
    v->n = n;
    return v;
}

//before one
Value* make_closure_val(char* param, Expr* body, Env* ds) {
    Value* v = (Value*)calloc(1, sizeof(Value));
    v->type = V_CLOSURE;
    v->param = param;
    v->body = body;
    v->ds = ds;
    return v;
}

/*
[solve myself]: N, Gemini
[Time Taken]: 13
[contract]: make_thunk: Expr*, Env* -> Thunk*
[purpose]: Thunk 생성 cache는 NULL.
[tests]:
    make_thunk(expr, env) -> Thunk(expr, env, NULL)
*/
Thunk* make_thunk(Expr* expr, Env* ds) {
    Thunk* t = (Thunk*)calloc(1, sizeof(Thunk));
    t->expr = expr;
    t->ds = ds;
    t->cache = NULL;
    return t;
}

/*
[solve myself]: N, Gemini
[Time Taken]: 14
[contract]: lookup: char*, Env* -> Thunk*
[purpose]: Env에서 해당 이름을 가진 Thunk 찾기
[tests]:
    lookup("x", {x: Thunk(1)}) -> Thunk(1)
    lookup("y", {x: Thunk(1)}) -> Error (Free Identifier)
*/
Thunk* lookup(char* name, Env* ds) {
    if (!ds) {
        printf("Free Identifier: %s\n", name);
        exit(1);
    }
    if (strcmp(ds->name, name) == 0) return ds->thunk;
    return lookup(name, ds->next);
}

// 선언먼저
Value* interp(Expr* expr, Env* ds);

/*
[solve myself]: N, Gemini
[Time Taken]: 9
[contract]: strict: Thunk* -> Value*
[purpose]: Thunk를 계산시킴
[tests]:
    strict(Thunk(Num(10))) -> Value(10) (and updates cache)
    strict(Thunk(Cached(10))) -> Value(10) (immediately)
*/
Value* strict(Thunk* t) {
    if (t->cache != NULL) {
        return t->cache;
    } else {
        Value* v = interp(t->expr, t->ds);
        t->cache = v;
        return v;
    }
}

//before one
Value* num_op(Value* l, Value* r, char op) {
    if (l->type != V_NUM || r->type != V_NUM) {
        printf("Error: Expected number\n");
        exit(1);
    }
    if (op == '+') return make_num_val(l->n + r->n);
    if (op == '-') return make_num_val(l->n - r->n);
    return NULL;
}

/*
[solve myself]: N, Gemini
[Time Taken]: 34
[contract]: interp: Expr*, Env* -> Value*
[purpose]: lazy 지원 interp
[tests]:
    interp({fun {x} 10}, mt) -> Closure
    interp({{fun {x} 10} {+ 1 {fun {y} y}}}, mt) -> Value(10) (Lazy Check)
*/
Value* interp(Expr* expr, Env* ds) {
    switch (expr->type) {
        case T_NUM: 
            return make_num_val(expr->n);

        case T_ADD: 
            return num_op(interp(expr->e1, ds), interp(expr->e2, ds), '+');

        case T_SUB: 
            return num_op(interp(expr->e1, ds), interp(expr->e2, ds), '-');

        case T_ID:  
            return strict(lookup(expr->name, ds));

        case T_FUN: 
            return make_closure_val(expr->name, expr->e1, ds);

        case T_APP: {
            Value* f_val = interp(expr->e1, ds);
            if (f_val->type != V_CLOSURE) {
                printf("Error: App expects a function\n");
                exit(1);
            }

            Thunk* arg_thunk = make_thunk(expr->e2, ds);

            Env* new_env = (Env*)malloc(sizeof(Env));
            new_env->name = f_val->param;
            new_env->thunk = arg_thunk;
            new_env->next = f_val->ds; 

            return interp(f_val->body, new_env);
        }

        case T_IF: {
            Value* cond = interp(expr->e1, ds);
            if (cond->n != 0) return interp(expr->e2, ds); 
            else return interp(expr->e3, ds); 
        }

        case T_REC: {
            Thunk* rec_thunk = make_thunk(expr->e1, ds);

            Env* new_env = (Env*)malloc(sizeof(Env));
            new_env->name = expr->name;
            new_env->thunk = rec_thunk;
            new_env->next = ds;

            rec_thunk->ds = new_env;

            return interp(expr->e2, new_env);
        }
    }
    return NULL;
}

//before one
void print_value(Value* v) {
    if (v->type == V_NUM) printf("Result: %d\n", v->n);
    else printf("Result: <Closure>\n");
}


// ==========================================
// 5. Parser Implementation (Recursive Descent)
// ==========================================

char* input_ptr; // 파싱할 input

//설명 X
void skip_whitespace() {
    while (isspace(*input_ptr)) input_ptr++;
}

//꼭 필요한게 나오지 않으면 error ex) "}"가 나오지 않는다.
void consume(char expected) {
    skip_whitespace();
    if (*input_ptr == expected) {
        input_ptr++;
    } else {
        printf("Syntax Error: Expected '%c' but got '%c'\n", expected, *input_ptr);
        exit(1);
    }
}

//예상하면 1 아님 0
int peek(char expected) {
    skip_whitespace();
    return *input_ptr == expected;
}

// strncmp의 상위버전 다르면 0, 같은데 뒤에 쓰잘데기 없는거 있으면 0 리턴
int match_keyword(const char* keyword) {
    skip_whitespace();
    int len = strlen(keyword);
    if (strncmp(input_ptr, keyword, len) == 0) {
        char next = input_ptr[len];
        if (!isalnum(next) && next != '_') {
            input_ptr += len;
            return 1;
        }
    }
    return 0;
}

Expr* parse_expr(); // Forward declaration

//num을 parsing 함 end는 숫자들을 파싱하고 파싱되지 않은 마지막 pointer 이기에 input_ptr == end 이면 변환이 되지 않은 것 이다.
Expr* parse_num() {
    skip_whitespace();
    char* end;
    long val = strtol(input_ptr, &end, 10);
    if (input_ptr == end) {
        printf("Syntax Error: Expected number\n");
        exit(1);
    }
    input_ptr = end;
    return make_num((int)val);
}

//id를 str으로 return 함
char* parse_id_str() {
    skip_whitespace();
    char buffer[256];
    int i = 0;
    while (isalnum(*input_ptr) || *input_ptr == '_') {
        buffer[i++] = *input_ptr++;
    }
    buffer[i] = '\0';
    if (i == 0) {
        printf("Syntax Error: Expected identifier\n");
        exit(1);
    }
    return strdup(buffer);
}

//parse_id_str으로 만든 str을 make_id 로 넣어 id를 만든다
Expr* parse_id() {
    return make_id(parse_id_str());
}

//메인 파서
Expr* parse_expr() {
    skip_whitespace();

    // 1. Number
    if (isdigit(*input_ptr) || (*input_ptr == '-' && isdigit(input_ptr[1]))) {
        return parse_num();
    }

    // 2. Compound Expressions { ... }
    if (*input_ptr == '{') {
        consume('{');
        Expr* ast = NULL;

        if (match_keyword("+")) {
            Expr* l = parse_expr();
            Expr* r = parse_expr();
            ast = make_add(l, r);
        }
        else if (match_keyword("-")) {
            Expr* l = parse_expr();
            Expr* r = parse_expr();
            ast = make_sub(l, r);
        }

        else if (match_keyword("if0")) {
            Expr* cond = parse_expr();
            Expr* then_e = parse_expr();
            Expr* else_e = parse_expr();
            ast = make_if(cond, then_e, else_e);
        }
        else if (match_keyword("fun")) {
            // {fun {param} body}
            consume('{');
            char* param = parse_id_str();
            consume('}');
            Expr* body = parse_expr();
            ast = make_fun(param, body);
        }
        else if (match_keyword("with")) {
            // {with {x v} body} -> Desugar to {{fun {x} body} v}
            consume('{');
            char* name = parse_id_str();
            Expr* val = parse_expr();
            consume('}');
            Expr* body = parse_expr();
            // Create App(Fun(name, body), val)
            ast = make_app(make_fun(name, body), val);
        }
        else if (match_keyword("rec")) {
            // {rec {name val} body}
            consume('{');
            char* name = parse_id_str();
            Expr* val = parse_expr();
            consume('}');
            Expr* body = parse_expr();
            ast = make_rec(name, val, body);
        }
        else {
            // Function Application {f arg}
            Expr* ftn = parse_expr();
            Expr* arg = parse_expr();
            ast = make_app(ftn, arg);
        }

        consume('}');
        return ast;
    }

    // 3. 숫자도, {}도 아니면 변수다 Identifier
    return parse_id();
}

//str를 전역변수 input_ptr에 넣고 Expr*를 반환하는 parse_expr를 수환
Expr* parse(char* str) {
    input_ptr = str;
    return parse_expr();
}

// Helper to print AST (for -p option)
void print_expr(Expr* e) {
    switch(e->type) {
        case T_NUM: printf("Num(%d)", e->n); break;
        case T_ADD: printf("Add("); print_expr(e->e1); printf(","); print_expr(e->e2); printf(")"); break;
        case T_SUB: printf("Sub("); print_expr(e->e1); printf(","); print_expr(e->e2); printf(")"); break;
        case T_ID:  printf("Id(%s)", e->name); break;
        case T_FUN: printf("Fun(%s,", e->name); print_expr(e->e1); printf(")"); break;
        case T_APP: printf("App("); print_expr(e->e1); printf(","); print_expr(e->e2); printf(")"); break;
        case T_IF:  printf("If0("); print_expr(e->e1); printf(","); print_expr(e->e2); printf(","); print_expr(e->e3); printf(")"); break;
        case T_REC: printf("Rec(%s,", e->name); print_expr(e->e1); printf(","); print_expr(e->e2); printf(")"); break;
    }
}

// ==========================================
// 6. Main CLI
// ==========================================

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: %s [-p] \"<code_string>\"\n", argv[0]);
        return 1;
    }

    int print_ast = 0;
    char* code = NULL;

    // Handle arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-p") == 0) {
            print_ast = 1;
        } else {
            code = argv[i];
        }
    }

    if (code == NULL) {
        printf("Error: No code provided.\n");
        return 1;
    }

    // Parse
    Expr* root = parse(code);

    // Option -p: Print AST only
    if (print_ast) {
        print_expr(root);
        printf("\n");
    } 
    // Default: Interpret
    else {
        Env* mt_env = NULL;
        Value* result = interp(root, mt_env);
        print_value(result);
    }

    return 0;
}