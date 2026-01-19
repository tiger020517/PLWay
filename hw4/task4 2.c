#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// [BNF Definition]
// <expr> ::= <num>
//          | {+ <expr> <expr>}
//          | {- <expr> <expr>}
//          | <id>
//          | {fun {<id>} <expr>}
//          | {<expr> <expr>}            // App
//          | {if <expr> <expr> <expr>}  // ifexp (not if0)
//          | {= <expr> <expr>}          // eq
//          | {or <expr> <expr>}         // orop
//          | {rec {<id> <expr>} <expr>} // Recursion
*/

#define T_NUM 0
#define T_ADD 1
#define T_SUB 2
#define T_ID  3
#define T_FUN 4
#define T_APP 5
#define T_IF  6   // ifexp
#define T_EQ  7   // =
#define T_OR  8   // orop
#define T_REC 9   // recursion

#define V_NUM 0
#define V_CLOSURE 1

#define ENV_MT 0
#define ENV_SUB 1




typedef struct Expr {
    int type;
    int n;
    char* name;      // T_ID, T_FUN param, T_REC name
    struct Expr* e1; // lhs, cond, rec-named-expr
    struct Expr* e2; // rhs, then, rec-body
    struct Expr* e3; // else (for if)
} Expr;

typedef struct Value {
    int type;
    int n;
    char* param;
    Expr* body;
    Env* ds;
} Value;


typedef struct Env {
    int type;
    char* name;
    Value* value;
    struct Env* next;
} Env;


/*
[solve myself]: N, task3 + gemini
[Time Taken]: 13
[contract]: Expr: int(type) -> Expr
[purpose]: Type을 받아 빈 Expr return
[tests]:
    new_expr(0) -> type에 0이 들어가고 나머지가 초기화 된 Expr 구조체
    new_expr(1) -> type에 1이 들어가고 나머지가 초기화 된 Expr 구조체
    new_expr(2) -> type에 2가 들어가고 나머지가 초기화 된 Expr 구조체
*/
Expr* new_expr(int type) {
    Expr* e = (Expr*)calloc(1, sizeof(Expr));
    e->type = type;
    return e;
}


/*
[solve myself]: Y
[Time Taken]: 5
[contract]: make_num: String -> Expr
[purpose]: int n을 받아 type이 num이고 n의 값이 들어간 expr을 return
[tests]:
    make_num(3) -> Expr(type = T_NUM, n = 3)
    make_num(5) -> Expr(type = T_NUM, n = 5)
    make_num(9) -> Expr(type = T_NUM, n = 9)
*/
Expr* make_num(int n) {
    Expr* e = new_expr(T_NUM);
    e->n = n;
    return e;
}


/*
[solve myself]: Y
[Time Taken]: 7
[contract]: make_add: l, r -> Expr
[purpose]: l Expr과 r Expr를 더하는 Expr을 return
[tests]:
    make_add(Expr(type = T_NUM, n = 3), Expr(type = T_NUM, n = 3)) -> Expr(type = T_ADD, e1 = Expr(type = T_NUM, n = 3), e2 = Expr(type = T_NUM, n = 3)))
    make_add(Expr(type = T_NUM, n = 4), Expr(type = T_NUM, n = 6)) -> Expr(type = T_ADD, e1 = Expr(type = T_NUM, n = 4), e2 = Expr(type = T_NUM, n = 6)))
    make_add(Expr(type = T_NUM, n = 5), Expr(type = T_NUM, n = 7)) -> Expr(type = T_ADD, e1 = Expr(type = T_NUM, n = 5), e2 = Expr(type = T_NUM, n = 7)))
*/
Expr* make_add(Expr* l, Expr* r) {
    Expr* e = new_expr(T_ADD);
    e->e1 = l; e->e2 = r;
    return e;
}


/*
[solve myself]: Y
[Time Taken]: 3
[contract]: make_sub: l, r -> Expr
[purpose]: l Expr과 r Expr를 빼는 Expr을 return
[tests]:
    make_sub(Expr(type = T_NUM, n = 3), Expr(type = T_NUM, n = 3)) -> Expr(type = T_SUB, e1 = Expr(type = T_NUM, n = 3), e2 = Expr(type = T_NUM, n = 3)))
    make_sub(Expr(type = T_NUM, n = 4), Expr(type = T_NUM, n = 6)) -> Expr(type = T_SUB, e1 = Expr(type = T_NUM, n = 4), e2 = Expr(type = T_NUM, n = 6)))
    make_sub(Expr(type = T_NUM, n = 5), Expr(type = T_NUM, n = 7)) -> Expr(type = T_SUB, e1 = Expr(type = T_NUM, n = 5), e2 = Expr(type = T_NUM, n = 7)))
*/
Expr* make_sub(Expr* l, Expr* r) {
    Expr* e = new_expr(T_SUB);
    e->e1 = l; e->e2 = r;
    return e;
}


/*
[solve myself]: Y
[Time Taken]: 4
[contract]: make_id: char* -> Expr
[purpose]: char*를 받아 해당 이름을 가진 id type의 Expr을 return함
[tests]:
    make_id("babo") -> Expr(type = T_ID, name = "babo")
    make_id("junhy") -> Expr(type = T_ID, name = "junhy")
    make_id("love") -> Expr(type = T_ID, name = "love")
*/
Expr* make_id(char* name) {
    Expr* e = new_expr(T_ID);
    e->name = strdup(name);
    return e;
}


/*
[solve myself]: y
[Time Taken]: 3
[contract]: make_fun: char*, body -> Expr
[purpose]: param과 body를 받아 T_FUN type의 Expr에 넣어서 return
[tests]:
    make_fun("addone", make_add(make_id("x"), make_num(1))) -> Expr(type = T_FUN, name = addone, body = make_add(make_id("x"), make_num(1)))
    make_fun("subone", make_sub(make_id("x"), make_num(1))) -> Expr(type = T_FUN, name = subone, body = make_sub(make_id("x"), make_num(1)))
*/
Expr* make_fun(char* param, Expr* body) {
    Expr* e = new_expr(T_FUN);
    e->name = strdup(param);
    e->e1 = body;
    return e;
}


/*
[solve myself]: N, gemini + task3
[Time Taken]: 3
[contract]: make_app: Expr, Expr -> Expr
[purpose]: 함수와 매개변수를 받아 해당하는 Expr을 반환
[tests]:
    make_app(addone, 1) -> Expr(type = T_app, e1 = addone, e2 = 1)
    make_app(subtwo, 1) -> Expr(type = T_app, e1 = subtwo, e2 = 1)
*/
Expr* make_app(Expr* ftn, Expr* arg) {
    Expr* e = new_expr(T_APP);
    e->e1 = ftn; e->e2 = arg;
    return e;
}


/*
[solve myself]: N, tak3 + gemini
[Time Taken]: 5
[contract]: make_if: Expr, Expr, Expr -> Expr
[purpose]: 조건과 참 / 거짓일 떄의 표현식을 받아 해당하는 Expr을 반환함
[tests]:
    make_if(make_id("cond"), make_num(1), make_num(0)) -> Expr(type = T_IF, e1 = Expr(T_ID, "cond"), e2 = Expr(T_NUM, 1), e3 = Expr(T_NUM, 0))
    make_if(make_eq(make_id("x"), make_num(5)), make_id("true_branch"), make_id("false_branch")) -> Expr(type = T_IF, e1 = Expr(type = T_EQ), e2 = Expr(T_ID, "true_branch"), e3 = Expr(T_ID, "false_branch"))
*/
Expr* make_if(Expr* cond, Expr* then_e, Expr* else_e) {
    Expr* e = new_expr(T_IF);
    e->e1 = cond; e->e2 = then_e; e->e3 = else_e;
    return e;
}


/*
[solve myself]: y
[Time Taken]: 3
[contract]: make_eq: Expr, Expr -> Expr
[purpose]: l과 r이 같은지 틀린지 if의 조건식으로 쓰이는 Expr를 반환
[tests]:
    make_eq(make_num(10), make_num(20)) -> Expr(type = T_EQ, e1 = Expr(T_NUM, 10), e2 = Expr(T_NUM, 20))
    make_eq(make_id("x"), make_num(0)) -> Expr(type = T_EQ, e1 = Expr(T_ID, "x"), e2 = Expr(T_NUM, 0))
*/
Expr* make_eq(Expr* l, Expr* r) {
    Expr* e = new_expr(T_EQ);
    e->e1 = l; e->e2 = r;
    return e;
}


/*
[solve myself]: y
[Time Taken]: 3
[contract]: make_or: String -> Expr
[purpose]: 논리합의 Expr을 만듦
[tests]:
    make_or(make_id("is_adult"), make_id("has_ticket")) -> Expr(type = T_OR, e1 = Expr(T_ID, "is_adult"), e2 = Expr(T_ID, "has_ticket"))
    make_or(make_eq(make_id("n"), make_num(0)), make_eq(make_id("n"), make_num(1))) -> Expr(type = T_OR, e1 = Expr(T_EQ, ...), e2 = Expr(T_EQ, ...))
*/
Expr* make_or(Expr* l, Expr* r) {
    Expr* e = new_expr(T_OR);
    e->e1 = l; e->e2 = r;
    return e;
}


/*
[solve myself]: N
[Time Taken]: 6
[contract]: make_rec: char*, Expr, Expr -> Expr
[purpose]: rec 구문을 만듦
[tests]:
    make_rec("cnt", make_fun("x", make_id("x")), make_app(make_id("cnt"), make_num(10))) -> Expr(type = T_REC, name = "cnt", e1 = Expr(T_FUN, ...), e2 = Expr(T_APP, ...))
    make_rec("fib", make_fun("n", make_if(...)), make_app(make_id("fib"), make_num(5))) -> Expr(type = T_REC, name = "fib", e1 = Expr(T_FUN, ...), e2 = Expr(T_APP, ...))
*/
Expr* make_rec(char* name, Expr* named_expr, Expr* body) {
    Expr* e = new_expr(T_REC);
    e->name = strdup(name);
    e->e1 = named_expr; 
    e->e2 = body;       
    return e;
}


/*
[solve myself]: y
[Time Taken]: 3
[contract]: make_num_val: int -> Value
[purpose]: n값을 가진 value 구조체 반환
[tests]:
    make_num_val(4) -> Value(type = 0, n = 4)
    make_num_val(9) -> Value(type = 0, n = 9)
*/
Value* make_num_val(int n) {
    Value* v = (Value*)calloc(1, sizeof(Value));
    v->type = V_NUM;
    v->n = n;
    return v;
}


/*
[solve myself]: n gemini
[Time Taken]: 14
[contract]: make_closure_val: char *, Expr, Env -> Value
[purpose]: 클로저를 반환
[tests]:
    make_closure_val(fun_ast->name, fun_ast->e1, mt_env) 
        -> Value(
            type = V_CLOSURE, 
            param = "a", 
            body = Expr(T_ADD, E1=T_ID("a"), E2=T_NUM(1)), 
            ds = Env(T_MT)
        )
*/
Value* make_closure_val(char* param, Expr* body, Env* ds) {
    Value* v = (Value*)calloc(1, sizeof(Value));
    v->type = V_CLOSURE;
    v->param = param;
    v->body = body;
    v->ds = ds;
    return v;
}

/*
[solve myself]: N gemini
[Time Taken]: 16
[contract]: isRecursion: Expr -> 0 or 1
[purpose]: Tree 안에 Recursion이 있는지 확인
[tests]:
    NUM or ID일 경우 Tree 가 가지 않아 0
    Parameter에 따라 검사
*/
int isRecursion(Expr* e) {
    if (!e) return 0;
    switch (e->type) {
        case T_REC: return 1;
        case T_NUM: 
        case T_ID: return 0;
        case T_ADD:
        case T_SUB:
        case T_EQ:
        case T_OR:
        case T_APP:
            return isRecursion(e->e1) || isRecursion(e->e2);
        case T_FUN:
            return isRecursion(e->e1);
        case T_IF:
            return isRecursion(e->e1) || isRecursion(e->e2) || isRecursion(e->e3);
    }
    return 0;
}


/*
[solve myself]: N gemini
[Time Taken]: 29
[contract]: create_mk_rec: Expr -> app's 결과물 Expr
[purpose]: mk_rec을 구현하는 함수
[tests]:
    너무길다 꾀꼬리
*/
Expr* create_mk_rec(Expr* body_proc) {
    // Part A: {fun {fX} {fX fX}}
    Expr* partA = make_fun("fX", make_app(make_id("fX"), make_id("fX")));
    
    // Part B inner: {{fX fX} x}
    Expr* apply_self = make_app(make_app(make_id("fX"), make_id("fX")), make_id("x"));
    
    // Part B: {fun {x} {{fX fX} x}} -> Wraps recursion to delay execution (essential for CBV)
    Expr* delayed_call = make_fun("x", apply_self);

    // Part C: {body-proc {fun {x} ...}}
    Expr* body_app = make_app(body_proc, delayed_call);

    // Part D: {fun {fX} ...}
    Expr* partD = make_fun("fX", body_app);
    
    // Combine
    return make_app(partA, partD);
}


/*
[solve myself]: N, gemini
[Time Taken]: 0 (create_mk_rec이랑 같이 풀어줌)
[contract]: desugar: Expr -> Expr
[purpose]: desugaring을 함
[tests]: NULL
*/
Expr* desugar(Expr* e) {
    if (!e) return NULL;

    switch (e->type) {
        case T_NUM: return make_num(e->n);
        case T_ID:  return make_id(e->name);
        case T_ADD: return make_add(desugar(e->e1), desugar(e->e2));
        case T_SUB: return make_sub(desugar(e->e1), desugar(e->e2));
        case T_EQ:  return make_eq(desugar(e->e1), desugar(e->e2));
        case T_OR:  return make_or(desugar(e->e1), desugar(e->e2));
        case T_APP: return make_app(desugar(e->e1), desugar(e->e2));
        case T_FUN: return make_fun(e->name, desugar(e->e1));
        case T_IF:  return make_if(desugar(e->e1), desugar(e->e2), desugar(e->e3));
        
        case T_REC: {
            // Logic: {rec {f v} b}
            // => {with {f {mk-rec {fun {f} v}}} b}
            // => {{fun {f} b} {mk-rec {fun {f} v}}}
            
            char* f_name = e->name;
            Expr* v_expr = desugar(e->e1); // v
            Expr* b_expr = desugar(e->e2); // b

            // 1. {fun {f} v}
            Expr* body_proc = make_fun(f_name, v_expr);

            // 2. {mk-rec ...}
            Expr* fix_expr = create_mk_rec(body_proc);

            // 3. {fun {f} b}
            Expr* main_fun = make_fun(f_name, b_expr);

            // 4. App
            return make_app(main_fun, fix_expr);
        }
    }
    return NULL;
}


/*
[solve myself]: N gemini
[Time Taken]: 23
[contract]: lookup: char *, Env -> Value
[purpose]: 환경에서 해당 이름을 가진 환경을 찾음
[tests]: NULL
*/
Value* lookup(char* name, Env* ds) {
    if (!ds) {
        printf("Free Identifier: %s\n", name);
        exit(1);
    }
    if (strcmp(ds->name, name) == 0) return ds->value;
    return lookup(name, ds->next);
}


/*
[solve myself]: N gemini
[Time Taken]: 2
[contract]: num_op: Value, Value, char(op) -> Expr(with ops)
[purpose]: RCFAE 언어의 문자열을 받아 AST(Abstract Syntax Tree)를 반환함
[tests]:NULL
*/
Value* num_op(Value* l, Value* r, char op) {
    if (l->type != V_NUM || r->type != V_NUM) {
        printf("Error: Expected number\n");
        exit(1);
    }
    if (op == '+') return make_num_val(l->n + r->n);
    if (op == '-') return make_num_val(l->n - r->n);
    if (op == '=') return make_num_val(l->n == r->n ? 1 : 0);
    return NULL;
}


/*
[solve myself]: N
[Time Taken]: 17
[contract]: interp: Expr, ds -> Expr or Val
[purpose]: Our Interpreter
[tests]: Null
*/
Value* interp(Expr* expr, Env* ds) {
    switch (expr->type) {
        case T_NUM: return make_num_val(expr->n);
        case T_ID:  return lookup(expr->name, ds);
        case T_ADD: return num_op(interp(expr->e1, ds), interp(expr->e2, ds), '+');
        case T_SUB: return num_op(interp(expr->e1, ds), interp(expr->e2, ds), '-');
        case T_EQ:  return num_op(interp(expr->e1, ds), interp(expr->e2, ds), '=');
        
        case T_OR: {
            Value* v1 = interp(expr->e1, ds);
            if (v1->n != 0) return make_num_val(1);
            Value* v2 = interp(expr->e2, ds);
            return (v2->n != 0) ? make_num_val(1) : make_num_val(0);
        }

        case T_IF: {
            Value* cond = interp(expr->e1, ds);
            if (cond->n != 0) return interp(expr->e2, ds); 
            else return interp(expr->e3, ds); 
        }

        case T_FUN: return make_closure_val(expr->name, expr->e1, ds);

        case T_APP: {
            Value* f_val = interp(expr->e1, ds);
            Value* a_val = interp(expr->e2, ds);
            if (f_val->type != V_CLOSURE) {
                printf("Error: App expects a function\n");
                exit(1);
            }
            Env* new_env = (Env*)malloc(sizeof(Env));
            new_env->name = f_val->param;
            new_env->value = a_val;
            new_env->next = f_val->ds;
            new_env->type = ENV_SUB;
            return interp(f_val->body, new_env);
        }

        case T_REC:
            printf("Error: T_REC found in interp! Run desugar() first.\n");
            exit(1);
    }
    return NULL;
}











/*
[solve myself]: N gemini
[Time Taken]: 0
[contract]: ???
[purpose]: Gemini 생성 test용 main code와 print_value 함수
[tests]: It's up to you
*/
void print_value(Value* v) {
    if (v->type == V_NUM) printf("Result: %d\n", v->n);
    else printf("Result: <Closure>\n");
}

int main() {
    Env* mt_env = NULL;

    printf("--- Test: Fibonacci (Clean using if, or, =) ---\n");
    /*
      {rec {fib {fun {n} 
                  {if {or {= n 0} {= n 1}}
                      1 
                      {+ {fib {- n 1}} {fib {- n 2}}}
                  }}} 
           {fib 6}}
      Expected: 13 (Sequence: 1, 1, 2, 3, 5, 8, 13)
    */
    
    // Condition: {or {= n 0} {= n 1}}
    Expr* cond = make_or(
        make_eq(make_id("n"), make_num(0)),
        make_eq(make_id("n"), make_num(1))
    );

    // Recursive Step: {+ {fib {- n 1}} {fib {- n 2}}}
    Expr* rec_step = make_add(
        make_app(make_id("fib"), make_sub(make_id("n"), make_num(1))),
        make_app(make_id("fib"), make_sub(make_id("n"), make_num(2)))
    );

    // Body: {if cond 1 rec_step}
    Expr* fib_body = make_if(cond, make_num(1), rec_step);

    // Rec: {rec {fib {fun {n} ...}} ...}
    Expr* rfae_expr = make_rec(
        "fib",
        make_fun("n", fib_body),
        make_app(make_id("fib"), make_num(6))
    );

    // 1. Check Recursion
    if (isRecursion(rfae_expr)) {
        printf("[INFO] Recursion detected. Desugaring...\n");
        // 2. Desugar (Transform RFAE to FAE)
        Expr* fae_expr = desugar(rfae_expr);
        // 3. Interp
        print_value(interp(fae_expr, mt_env));
    } else {
        printf("[INFO] No recursion detected.\n");
        print_value(interp(rfae_expr, mt_env));
    }

    return 0;
}