open Typechecker
open Semantics


(* Esempio dalla issue, dovrebbe essere permesso *)
let%test "test_div_ok" = test_typecheck
  "contract C {
    int x;
    function f(int y) public returns(int) { x = y * ((1 / 3) * 6); }
  }"
true

let%test "test_constant_int_if_false" = test_typecheck
"contract C {
      int x;
      function f(int y) public returns(int) { x =  (2 / 3); }
}"
false

let%test "test_constant_int_if_falseb" = test_typecheck
"contract C {
      int x;
      function f(int y) public returns(int) { x = y* (2 / 3); }
}"
false

(* Divisione per 0 con prodotto *)
let%test "test_div_0_times" = test_typecheck
  "contract C {
    int x;
    function f(int y) public returns(int) { x = 2 * ((1 / 0) * 6); }
  }"
false


(* Divisione per 0 *)
let%test "test_div_0" = test_typecheck
  "contract C {
    int x;
    function f(int y) public returns(int) { x = (1 / 0); }
  }"
false


(* Divisione per 0 con una variabile *)
let%test "test_div_0_var" = test_typecheck
  "contract C {
    int x;
    int y = 2;
    function f(int y) public returns(int) { x = (y / 0); }
  }"
false


(* Divisione per 0 con due variabili *)
let%test "test_div_0_var_var" = test_typecheck
  "contract C {
    int x;
    int y = 2;
    int z = 0;
    function f(int y) public returns(int) { x = (y/ z); }
  }"
true


(* Divisione normale tra due variabili *)
let%test "test_div_ok_var_var" = test_typecheck
  "contract C {
    int x = 2;
    int y = 2;
    function f(int y) public returns(int) { x = (y/x); }
  }"
true


(* Divisione normale tra due costanti *)
let%test "test_div_ok_const_const" = test_typecheck
  "contract C {
    int x=5;
    function f() public returns(int) { x = (4/2); }
  }"
true


let%test "test_constant_int_if_true3" = test_typecheck
  "contract C {
      int x=2;
      int y = 2;
int z =2;
      function f(int y) public returns(int) { x = z* (y/ x); }
}"
true


  let%test "test_constant_int_if_true4" = test_typecheck
   "contract C {
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = (4/2); }
}"
true
(* ??? ma non esistono i float *)
(*----------chiedere qua speigazion ?????????????????????*)
(*let%test "test_div_???" = test_typecheck
  "contract C {
    uint x= 5.98 * 100;
    int y = 2;
    int z= 4;
    function f() public returns(int) { y = x/z; }
  }"
true*)


(* Divisione con tipo non numerico *)
let%test "test_div_bool" = test_typecheck
  "contract C {
    bool x;
    bool y = true;
    int z = 5;
    function f(int y) public returns(int) { x = (y / z); }
  }"
false


  (*segno meno,deve fallire il typechecker perchè provo ad assegnare ad un uint (senza segno)
  delle costanti con segno*)
  let%test "test_constant_int_if_false" = test_typecheck
   "contract C {
      uint x;
    
      function f() public returns(int) { x = (-10/2); }
}"
false

  (*segno meno*)
  let%test "test_constant_int_if_false" = test_typecheck
   "contract C {
      uint x;
      int y=10;
      int z=2;
    
      function f() public returns(int) { x = (-y/z); }
}"
false



(*-------------------TYPECHECKER OK-----------------------*)


(*-------------------TEST INTERPRETE----------------------*)

(* L'assegnamento non avviene perché la divisione per 0 viene bloccata dal typechecker *)
let%test "test_div_blocked_by_typechecker" = test_exec_tx
  "contract C {
    int x=1;
    int y = 2;
    int z= 0;
    function f() public returns(int) { x = (y/ z); }
  }"
  ["0xA:0xC.f()"]
  [("x==1");]


let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      function f() public returns(int) { x = (-4/2); }
}"
["0xA:0xC.f()"]
  [("x==-2");]


 let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = z*(y/z); }
}"
["0xA:0xC.f()"]
  [("x==2");]


  let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = y*(z/4); }
}"
["0xA:0xC.f()"]
  [("x==2");]


let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      int y = 9;
      int z= 2;
      function f() public returns(int) { x = -z*(y/z); }
}"
["0xA:0xC.f()"]
  [("x==-9");]
    
(*segno meno*)
  let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      int y = 9;
      int z= 2;
      function f() public returns(int) { x = (-10/2); }
}"
["0xA:0xC.f()"]
  [("x==-5");]



   let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x;
      int y = 9;
      int z= 2;
      function f() public returns(int) { x = (-y*z); }
}"
["0xA:0xC.f()"]
  [("x==-18");]

 

    







(* Divisione normale tra due variabili *)
let%test "test_div_ok_good_result" = test_exec_tx
  "contract C {
    int x=5;
    int y = 2;
    int z= 4;
    function f() public returns(int) { x = (4/2); }
  }"
  ["0xA:0xC.f()"]
  [("x==2");]


(* Divisione unita a una moltiplicazione *)
let%test "test_div_mul_var" = test_exec_tx
  "contract C {
    int x=5;
    int y = 2;
    int z= 4;
    function f() public returns(int) { x = y*(y/z); }
  }"
  ["0xA:0xC.f()"]
  [("x==1");]

(* Espressione che dovrebbe essere semplificata *)
let%test "test_mul_div_mix" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = 9*(1/3); }
  }"
  ["0xA:0xC.f()"]
  [("x==3");]


let%test "test_div_mul_mix" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = (1/3)*9; }
  }"
  ["0xA:0xC.f()"]
  [("x==3");]


let%test "test_mul_div_mix_a" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = 9*(2/3); }
  }"
  ["0xA:0xC.f()"]
  [("x==6");]


let%test "test_mul_div_mix_b" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = 7*(2/3); }
  }"
  ["0xA:0xC.f()"]
  [("x==-4");]

(* Questa invece funziona. Forse risolvere quella sopra romperebbe questa *)
let%test "test_mul_div_mix_c" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = 7*(2/3)*3; }
  }"
  ["0xA:0xC.f()"]
  [("x==14");]

(* Esempi della issue *)
let%test "test_div_issue_true" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y * ((1 / 3) * 6); }
  }"
true
let%test "test_div_issue_false" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y * (2 / 3); }
  }"
false

let%test "test_div_ok_good_result" = test_exec_tx
  "contract C {
    int x=111;
    int y = 2;
    int z= 4;
    function f() public returns(int) { x = 2*(5/4)*2; }
  }"
  ["0xA:0xC.f()"]
  [("x==5");]

let%test "test_riki" = test_exec_tx
  "contract C {
  int x;
  function f() public { x = (11/3)*11; }
  }"
  ["0xA:0xC.f()"]
  [("x==40");]

(* strano non funziona *)
let%test "test_mul_div_mix_b_strano_negativo" = test_exec_tx
  "contract C {
    int x=1;
    function f() public returns(int) { x = 7*(-2/3); }
  }"
  ["0xA:0xC.f()"]
  [("x==-4");]

(* Questo è unparsable??? Boh. test_mul_div_mix_b_strano_negativo_variabile threw (Failure "account 0xC unbound"). *)
(*let%test "test_mul_div_mix_b_strano_negativo_variabile" = test_exec_tx
  "contract C {
    int x=1;
    int y = -2;
    function f() public returns(int) { x = 7*(y/3); }
  }"
  ["0xA:0xC.f()"]
  [("x==-4");]*)