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

let%test "test_issue_11_strano" = test_typecheck
  "contract C {
    int constant N=1;
    int x=1;
    function f(int n) external { if (x==2) x+=1; else N=0; }
  }"
  false


