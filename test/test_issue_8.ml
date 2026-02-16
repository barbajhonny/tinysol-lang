open Typechecker

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
let%test "test_constant_bool_ok" = test_typecheck
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