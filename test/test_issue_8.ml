
open Typechecker

let%test "test_typecheck_constant_nostri_1" = test_typecheck
  "contract C {
    int constant N;
    constructor() { N = 3; } 
    function f(int n) external { N = 2; }
  }"
  false

let%test "test_typecheck_constant_nostri_2" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external {}
  }"
  true

let%test "test_typecheck_constant_nostri_2a" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() {} 
    function f(int n) external { N = 2+3;}
  }"
  false

(* altro tipo di assegnazione *)
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

  (* TODO: non funzionano (più?) gli immutable *)
let%test "test_typecheck_constant_nostri_immutable" = test_typecheck
  "contract C {
    int immutable N;
    constructor() {} 
    function f(int n) external {N = 3;}
  }"
  true