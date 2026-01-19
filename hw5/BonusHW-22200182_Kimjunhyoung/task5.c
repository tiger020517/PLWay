#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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

// 선언 먼저
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


// test용 main 문 from gemini
int main() {
    Env* mt_env = NULL;

    printf("--- Test 1: Basic Arithmetic ---\n");
    // {+ 10 20}
    Expr* test1 = make_add(make_num(10), make_num(20));
    print_value(interp(test1, mt_env));

    printf("\n--- Test 2: Lazy Evaluation Check ---\n");
    // Code: {{fun {x} 10} {+ 1 {fun {y} y}}}
    // Eager would fail (adding number to function). Lazy succeeds.
    Expr* lazy_expr = make_app(
        make_fun("x", make_num(10)),                 
        make_add(make_num(1), make_fun("y", make_id("y"))) 
    );
    print_value(interp(lazy_expr, mt_env));

    printf("\n--- Test 3: Recursion (Factorial like sum) ---\n");
    // Logic: {rec {f {fun {n} {if n {+ n {f {- n 1}}} 0}}} {f 5}}
    // 5 + 4 + 3 + 2 + 1 + 0 = 15
    Expr* rec_body = make_if(
        make_id("n"),
        make_add(make_id("n"), make_app(make_id("fac"), make_sub(make_id("n"), make_num(1)))),
        make_num(0)
    );
    Expr* rec_test = make_rec(
        "fac",
        make_fun("n", rec_body),
        make_app(make_id("fac"), make_num(5))
    );
    print_value(interp(rec_test, mt_env));

    return 0;
}