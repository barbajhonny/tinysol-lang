open Typechecker
open Semantics

let%test "test_constant_int_if_true" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = y * ((1 / 3) * 6); }
}"
true

let%test "test_constant_int_if_false1" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = 2 * ((1 / 0) * 6); }
}"
false

let%test "test_constant_int_if_false2" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = (1 / 0); }
}"
false

let%test "test_constant_int_if_false3" = test_typecheck
  "contract C {
      int x;
int y = 2;
      function f(int y) public returns(int) { x = (y/ 0); }
}"
false


let%test "test_constant_int_if_true2" = test_typecheck
  "contract C {
      int x;
      int y = 2;
int z= 0;
      function f(int y) public returns(int) { x = (y/ z); }
}"
true


let%test "test_constant_int_if_false4" = test_typecheck
  "contract C {
      int x;
      uint y = 2;
      function f(int y) public returns(int) { x = (y/ 0); }
}"
false


let%test "test_constant_int_if_true3" = test_typecheck
  "contract C {
      int x=2;
      int y = 2;
      function f(int y) public returns(int) { x = (y/ x); }
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

(*----------chiedere qua speigazion ?????????????????????*)
  let%test "test_constant_int_if_true4" = test_typecheck
   "contract C {
      uint x= 5.98 * 100;
      int y = 2;
      int z= 4;
      function f() public returns(int) { y = x/z; }
}"
true


let%test "test_constant_int_if_true5" = test_typecheck
  "contract C {
      bool x;
      bool y = true;
      int z = 5;
      function f(int y) public returns(int) { x = (y / z); }
}"
false




(*-------------------TYPECHECKER OK-----------------------*)


(*-------------------TEST INTERPRETE----------------------*)

let%test "test_constant_int_if_false" = test_exec_tx
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
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = (4/2); }
}"
["0xA:0xC.f()"]
  [("x==2");]



 let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = y*(y/z); }
}"
["0xA:0xC.f()"]
  [("x==1");]


