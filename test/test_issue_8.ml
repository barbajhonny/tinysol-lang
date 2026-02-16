open Typechecker

(*  true perchè la costante è dichiarata secondo i criteri (dichiarazione + assegnamento)  *)
let%test "test_typecheck_constant_nostri_2" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {}
  }"
  true


(*  false perchè si prova a riassegnare la costante *)
let%test "test_typecheck_constant_nostri_2a" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external { N = 2;}
  }"
  false


(*  false perchè non è concesso non inizializzare una costante  *)
let%test "test_typecheck_constant_nostri_1" = test_typecheck
  "contract C {
    int constant N;
    constructor() { N = 3; } 
    function f(int n) external { N = 2; }
  }"
  false


(* false perchè non è concesso riassegnare una costante *)
let%test "test_typecheck_constant_nostri_3" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { N += 2;} 
    function f(int n) external { N -= 3;}
  }"
  false


(* altro tipo di variabile *)
let%test "test_typecheck_constant_nostri_4" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {} 
    function f(int n) external {}
  }"
  true

let%test "test_typecheck_constant_nostri_5" = test_typecheck
  "contract C {
    bool constant N;
    constructor() {} 
    function f(int n) external {}
  }"
  false

(*false non è concesso riassegnare una costante*)
let%test "test_typecheck_constant_nostri_6" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {N = false;} 
    function f(int n) external {}
  }"
  false


let%test "test_typecheck_constant_nostri_7" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {} 
    function f(int n) external {N = true;}
  }"
  false

  (*NON SAPPIAMO SE LASCIARLO O MENO*)
let%test "test_issue_11_strano" = test_typecheck
  "contract C {
    int constant N=1;
    int x=1;
    function f(int n) external { if (x==2) x+=1; else N=0; }
  }"
  false


(* === TEST NUOVI PIU' ORDINATI? === *)

(* Costante int usata correttamente *)
let%test "test_constant_int_ok" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {}
  }"
  true

(* Costante bool usata correttamente *)
let%test "test_constant_int_ok" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {} 
    function f(int n) external {}
  }"
  true

(* Mancata inizializzazione *)
let%test "test_constant_int_constructor" = test_typecheck
  "contract C {
    int constant N;
    constructor() {} 
    function f(int n) external {}
  }"
  false
let%test "test_constant_bool_constructor" = test_typecheck
  "contract C {
    bool constant N;
    constructor() {} 
    function f(int n) external {}
  }"
  false

(* Assegnamento nel constructor *)
let%test "test_constant_int_constructor" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {N=2;} 
    function f(int n) external {}
  }"
  false
let%test "test_constant_bool_constructor" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {N=false;} 
    function f(int n) external {}
  }"
  false

(* Assegnamento nella funzione *)
let%test "test_constant_int_function" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {N=2;}
  }"
  false
let%test "test_constant_bool_function" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {} 
    function f(int n) external {N=false;}
  }"
  false

(* Incremento *)
let%test "test_constant_increment_constructor" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {N+=1;} 
    function f(int n) external {}
  }"
  false
let%test "test_constant_increment_function" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {N+=1;}
  }"
  false

(* Assegnamento da espressione *)
let%test "test_constant_int_constructor_expr" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {N=2+3;} 
    function f(int n) external {}
  }"
  false
let%test "test_constant_int_function_expr" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {N=2+3;}
  }"
  false
let%test "test_constant_bool_function_expr" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {} 
    function f(int n) external {N=false||true;}
  }"
  false
let%test "test_constant_bool_constructor_expr" = test_typecheck
  "contract C {
    bool constant N=true;
    constructor() {N=false||true;} 
    function f(int n) external {}
  }"
  false

(* Assegnamento condizionale *)
let%test "test_constant_int_if_true" = test_typecheck
  "contract C {
    int constant N=1;
    int x = 0;
    constructor() {if(true) N=2; else x=2;} 
    function f(int n) external {}
  }"
  false
(* Qui è nel ramo che non viene eseguito *)
let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
    int constant N=1;
    int x = 0;
    constructor() {if(false) N=2; else x=2;} 
    function f(int n) external {}
  }"
  true