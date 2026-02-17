open Typechecker
open Semantics


(* Esempio dalla issue, dovrebbe essere permesso *)
let%test "test_div_ok" = test_typecheck
  "contract C {
    int x;
    function f(int y) public returns(int) { x = y * ((1 / 3) * 6); }
  }"
true


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


(* ??? ma non esistono i float *)
(*----------chiedere qua speigazion ?????????????????????*)
let%test "test_div_???" = test_typecheck
  "contract C {
    uint x= 5.98 * 100;
    int y = 2;
    int z= 4;
    function f() public returns(int) { y = x/z; }
  }"
true


(* Divisione con tipo non numerico *)
let%test "test_div_bool" = test_typecheck
  "contract C {
    bool x;
    bool y = true;
    int z = 5;
    function f(int y) public returns(int) { x = (y / z); }
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
let%test "test_div_mul_mix" = test_exec_tx
  "contract C {
    int x=5;
    int y = 2;
    int z= 4;
    function f() public returns(int) { x = y*(y/z); }
  }"
  ["0xA:0xC.f()"]
  [("x==1");]